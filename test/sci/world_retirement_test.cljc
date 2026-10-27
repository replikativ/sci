(ns sci.world-retirement-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.fork :as fork]
            [sci.impl.world :as world]))

(defn- forkable-context [opts]
  (sci/init (assoc opts :runtime-mode :forkable)))

(defn- failure [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable :cljs :default :cljd Object) error error)))

(defn- retired-error? [error]
  (boolean (and error (re-find #"retired" (ex-message error)))))

(deftest retirement-is-terminal-and-idempotent
  (let [ctx (forkable-context {})]
    (sci/eval-string* ctx "(def x 1)")
    (is (nil? (sci/retire! ctx)))
    (is (nil? (sci/retire! ctx)))
    (is (retired-error? (failure #(sci/eval-string* ctx "(+ 1 2)"))))
    (is (retired-error? (failure #(sci/fork ctx))))
    (is (retired-error? (failure #(sci/call-with-context ctx (constantly :unreachable)))))))

(deftest standard-context-retirement-is-a-noop
  (let [ctx (sci/init {})]
    (sci/eval-string* ctx "(def x 1)")
    (is (nil? (sci/retire! ctx)))
    (is (= 2 (sci/eval-string* ctx "(inc x)")))
    (is (= 1 (sci/eval-string* (sci/fork ctx) "x")))))

(deftest host-and-interpreted-intern-register-independent-roots
  (let [parent (forkable-context {})]
    (sci/intern parent 'user 'host-root 1)
    (sci/eval-string* parent "(intern 'user 'interpreted-root 2)")
    (sci/intern parent 'user 'unbound-root)
    (let [child (sci/fork parent)]
      (sci/intern child 'user 'host-root 10)
      (is (= 20 (sci/eval-string* child "(intern 'user 'interpreted-root 20) interpreted-root")))
      (is (= [1 2 false]
             (sci/eval-string* parent "[host-root interpreted-root (bound? #'unbound-root)]")))
      (is (= [10 20 false]
             (sci/eval-string* child "[host-root interpreted-root (bound? #'unbound-root)]"))))))

#?(:clj
   (deftest sibling-descriptors-beyond-source-capacity-remain-absent
     (let [parent (forkable-context {})
           _ (sci/eval-string* parent "(def baseline (atom 5))")
           source (:sci.impl/world parent)
           capacity (.length ^java.util.concurrent.atomic.AtomicReferenceArray
                             @(.-cells-holder source))
           child (sci/fork parent)]
       (sci/eval-string* child (str "(dotimes [i " (inc capacity) "] (atom i))"))
       (is (> (:next-slot @(.-registry source)) capacity))
       (let [sibling (sci/fork parent)]
         (is (= 5 (sci/eval-string* sibling "@baseline")))
         (is (= 6 (sci/eval-string* sibling "(swap! baseline inc)")))
         (is (= 5 (sci/eval-string* parent "@baseline")))))))

(deftest retiring-one-world-preserves-related-worlds
  (testing "a retired child does not change its parent or sibling"
    (let [parent (forkable-context {})]
      (sci/eval-string* parent "(def counter (atom 0))")
      (let [child (sci/fork parent)
            sibling (sci/fork parent)]
        (is (= 1 (sci/eval-string* child "(swap! counter inc)")))
        (sci/retire! child)
        (is (= 0 (sci/eval-string* parent "@counter")))
        (is (= 1 (sci/eval-string* sibling "(swap! counter inc)")))
        (is (retired-error? (failure #(sci/eval-string* child "@counter")))))))
  (testing "a child keeps its already-forked state after its parent retires"
    (let [parent (forkable-context {})]
      (sci/eval-string* parent "(def counter (atom 10)) (def pending (delay (swap! counter inc)))")
      (let [child (sci/fork parent)]
        (sci/retire! parent)
        (is (= 11 (sci/eval-string* child "@pending")))
        (is (= 12 (sci/eval-string* child "(swap! counter inc)")))
        (let [grandchild (sci/fork child)]
          (is (= 12 (sci/eval-string* grandchild "@counter"))))))))

(deftest retired-world-cannot-be-resurrected-through-host-primitives
  (let [ctx (forkable-context {})
        a (sci/eval-string* ctx "(atom 1)")
        pending (sci/eval-string* ctx "(delay 2)")
        touched? (atom false)]
    (sci/intern ctx 'user 'a a)
    (let [child (sci/fork ctx)]
      (sci/retire! ctx)
      (is (retired-error? (failure #(deref a))))
      (is (retired-error? (failure #(reset! a 2))))
      (is (retired-error? (failure #(swap! a (fn [x] (reset! touched? true) (inc x))))))
      (is (false? @touched?) "a rejected mutation does not run its callback")
      (is (retired-error? (failure #(deref pending))))
      (is (retired-error? (failure #(realized? pending))))
      (is (= 1 (sci/call-with-context child #(deref a))))
      (is (= 3 (sci/call-with-context child #(reset! a 3))))
      (is (= 3 (sci/eval-string* child "@a"))))))

(deftest retirement-inside-evaluation-is-rejected
  (let [target (atom nil)
        ctx (forkable-context {:bindings {'retire! #(sci/retire! @target)}})]
    (reset! target ctx)
    (is (re-find #"Cannot retire.*active evaluation"
                 (ex-message (failure #(sci/eval-string* ctx "(retire!)")))))
    (is (= :still-open (sci/eval-string* ctx ":still-open")))
    (is (nil? (sci/retire! ctx)))))

(deftest a-child-cannot-fall-back-into-a-retired-home-world
  (let [parent (forkable-context {})
        child (sci/fork parent)
        ;; The child snapshot predates this primitive. Reading it would need
        ;; the creation-world fallback, rather than a copied child cell.
        late (sci/eval-string* parent "(atom 7)")
        touched? (atom false)]
    (is (= 7 (sci/call-with-context child #(deref late))))
    (sci/retire! parent)
    (is (retired-error? (failure #(sci/call-with-context child (fn [] (deref late))))))
    (is (retired-error?
         (failure #(sci/call-with-context
                    child
                    (fn [] (swap! late (fn [x] (reset! touched? true) (inc x))))))))
    (is (false? @touched?))
    (is (= :live (sci/eval-string* child ":live")))))

(deftest nested-child-evaluation-cannot-retire-its-active-parent
  (let [parent-ref (atom nil)
        child-ref (atom nil)
        parent (forkable-context
                {:bindings {'retire-parent! #(sci/retire! @parent-ref)
                            'enter-child! #(sci/eval-string* @child-ref "(retire-parent!)")}})]
    (reset! parent-ref parent)
    (reset! child-ref (sci/fork parent))
    (is (re-find #"Cannot retire.*active evaluation"
                 (ex-message (failure #(sci/eval-string* parent "(enter-child!)")))))
    (is (= :parent-open (sci/eval-string* parent ":parent-open")))
    (is (= :child-open (sci/eval-string* @child-ref ":child-open")))))

#?(:clj
   (deftest retirement-waits-for-another-threads-evaluation
     (let [entered (promise)
           release (promise)
           requested (promise)
           ctx (forkable-context {:bindings {'block! #(do (deliver entered true) @release)}})
           evaluation (future (sci/eval-string* ctx "(block!)"))]
       (try
         (is (= true (deref entered 5000 ::timeout)))
         (let [retirement (future (deliver requested true) (sci/retire! ctx))]
           (try
             (is (= true (deref requested 5000 ::timeout)))
             (is (= ::waiting (deref retirement 50 ::waiting)))
             (deliver release :finished)
             (is (= :finished (deref evaluation 5000 ::timeout)))
             (is (nil? (deref retirement 5000 ::timeout)))
             (is (retired-error? (failure #(sci/eval-string* ctx ":late"))))
             (finally (future-cancel retirement))))
         (finally
           (deliver release :finished)
           (future-cancel evaluation))))))

;; Cache owners are supported where the runtime has weak owner references.
;; Lazy-sequence-specific owner reachability tests live with that primitive.
#?(:cljd nil
   :default
   (do
     (deftype TestCacheOwner [cache failures calls forks rejection]
       world/IForkableWorldCache
       (-fork-world-cache! [_ source target]
         (swap! forks conj [source target (world/selected-world)])
         (when (contains? @cache source)
           (swap! cache assoc target (get @cache source)))
         (when-let [error @rejection] (throw error)))
       world/IWorldCache
       (-retire-world-cache! [_ selected]
         (swap! calls conj selected)
         (if (pos? @failures)
           (do (swap! failures dec)
               (throw (ex-info "test cache cleanup failed" {:test-cache-failure true})))
           (swap! cache dissoc selected))))

     (defn- register-cache-owner! [ctx failures]
       (sci/call-with-context
        ctx
        (fn []
          (let [home (world/selected-world)]
            (world/register-world-cache!
             (TestCacheOwner. (atom {home :cached}) (atom failures) (atom []) (atom []) (atom nil)))))))

     (deftype RejectingFork [owner original target]
       fork/Forkable
       (fork-value [_]
         (let [selected (world/selected-world)]
           (reset! target selected)
           (swap! (.-cache owner) assoc selected :unpublished))
         (throw original)))

     (deftype ReentrantForkAction [action]
       fork/Forkable
       (fork-value [this] (action) this))

     (deftest copying-cannot-recursively-retire-or-copy-its-source
       (doseq [operation [sci/retire! sci/fork]]
         (let [parent (forkable-context {})]
           (sci/intern parent 'user 'resource
                       (ReentrantForkAction. #(operation parent)))
           (is (re-find #"active evaluation or copy"
                        (ex-message (failure #(sci/fork parent)))))
           (is (= :live (sci/eval-string* parent ":live"))))))

     #?(:clj
        (deftype HashRejectingValue []
          fork/Forkable
          (fork-value [this] this)
          Object
          (hashCode [_]
            (throw (ex-info "A root value must not be hashed during copying." {})))))

     #?(:clj
        (deftest copying-recognizes-handles-without-hashing-value-roots
          (let [parent (forkable-context {})
                value (HashRejectingValue.)]
            (sci/intern parent 'user 'resource value)
            (let [child (sci/fork parent)]
              (is (identical? value (sci/eval-string* child "resource")))
              (is (nil? (sci/retire! child)))
              (is (identical? value (sci/eval-string* parent "resource")))))))

     (deftest weak-cache-owners-do-not-reserve-dense-slots
       (let [ctx (forkable-context {})
             selected (:sci.impl/world ctx)
             registry (.-registry selected)
             slots-before (:next-slot @registry)
             descriptors-before (count (:managed-descriptors @registry))
             owner (register-cache-owner! ctx 0)]
         (is (= slots-before (:next-slot @registry)))
         (is (= descriptors-before (count (:managed-descriptors @registry))))
         (is (= [owner] (world/world-cache-owners selected)))
         (sci/call-with-context ctx #(world/register-world-cache! owner))
         (is (= [owner] (world/world-cache-owners selected)) "duplicate registration keeps one owner")
         (let [child (sci/fork ctx)
               target (:sci.impl/world child)]
           (is (= [[selected target target]] @(.-forks owner)))
           (is (= :cached (get @(.-cache owner) target)))
           (is (= :cached (get @(.-cache owner) selected)))
           (sci/retire! child)
           (sci/retire! ctx))))

     (deftest retirement-removes-only-the-selected-worlds-caches
       (let [parent (forkable-context {})
             owner (register-cache-owner! parent 0)
             child (sci/fork parent)
             parent-world (:sci.impl/world parent)
             child-world (:sci.impl/world child)]
         (swap! (.-cache owner) assoc child-world :cached)
         (sci/retire! child)
         (is (= {parent-world :cached} @(.-cache owner)))
         (is (= [child-world] @(.-calls owner)))
         (sci/retire! child)
         (is (= [child-world] @(.-calls owner)) "completed retirement does not rerun cleanup")
         (is (= :live (sci/eval-string* parent ":live")))))

     (deftest cache-retirement-failure-is-closed-and-retryable
       (let [ctx (forkable-context {})
             selected (:sci.impl/world ctx)
             owner (register-cache-owner! ctx 1)]
         (is (:test-cache-failure (ex-data (failure #(sci/retire! ctx)))))
         (is (= {selected :cached} @(.-cache owner)))
         (is (retired-error? (failure #(sci/eval-string* ctx ":late"))))
         (is (retired-error? (failure #(sci/fork ctx))))
         (is (nil? (sci/retire! ctx)))
         (is (empty? @(.-cache owner)))))

     (deftest failed-fork-preserves-cleanup-ownership-for-retry
       (let [parent (forkable-context {})
             parent-world (:sci.impl/world parent)
             owner (register-cache-owner! parent 2)
             original (ex-info "fork policy rejected value" {:test-rejection true})
             target (atom nil)]
         (sci/intern parent 'user 'resource (RejectingFork. owner original target))
         (let [error (failure #(sci/fork parent))]
           (is (= ::world/fork-cleanup-failed (:type (ex-data error))))
           (is (identical? original (:fork-error (ex-data error))))
           (is (:test-cache-failure (ex-data (:cleanup-error (ex-data error)))))
           (is (identical? original (ex-cause error)))
           (is (identical? @target (:recovery-world (ex-data error))))
           (is (not (identical? parent-world @target)) "copiers see the unpublished target, not the source")
           (is (= :unpublished (get @(.-cache owner) @target)))
           (is (= :parent-live (sci/eval-string* parent ":parent-live")))
           (let [second-error (failure #(sci/recover-fork! error))]
             (is (= ::world/fork-cleanup-failed (:type (ex-data second-error))))
             (is (identical? original (:fork-error (ex-data second-error))))
             (is (identical? @target (:recovery-world (ex-data second-error))))
             (is (identical? error (:previous-recovery-failure (ex-data second-error))))
             (is (nil? (sci/recover-fork! second-error)))
             (is (= {parent-world :cached} @(.-cache owner)))
             (is (= :still-live (sci/eval-string* parent ":still-live")))
             (is (nil? (sci/recover-fork! second-error)))))))

     (deftest failed-cache-copy-retains-its-unpublished-target
       (let [parent (forkable-context {})
             source (:sci.impl/world parent)
             owner (register-cache-owner! parent 1)
             original (ex-info "cache copy failed" {:cache-copy-failure true})]
         (reset! (.-rejection owner) original)
         (let [error (failure #(sci/fork parent))
               target (:recovery-world (ex-data error))]
           (is (= ::world/fork-cleanup-failed (:type (ex-data error))))
           (is (identical? original (:fork-error (ex-data error))))
           (is (= [[source target target]] @(.-forks owner)))
           (is (= :cached (get @(.-cache owner) target)))
           (is (nil? (sci/recover-fork! error)))
           (is (= {source :cached} @(.-cache owner)))
           (is (= :live (sci/eval-string* parent ":live"))))))

     (deftest a-fork-policy-failure-with-successful-cleanup-is-not-replaced
       (let [parent (forkable-context {})
             owner (register-cache-owner! parent 0)
             original (ex-info "original policy failure" {:original true})
             target (atom nil)]
         (sci/intern parent 'user 'resource (RejectingFork. owner original target))
         (is (identical? original (failure #(sci/fork parent))))
         (is (= {(:sci.impl/world parent) :cached} @(.-cache owner)))
         (is (= :live (sci/eval-string* parent ":live")))))

     (deftest arbitrary-errors-are-not-fork-recovery-capabilities
       (let [parent (forkable-context {})]
         (is (= ::world/invalid-fork-recovery
                (:type (ex-data (failure #(sci/recover-fork! (ex-info "ordinary error" {})))))))
         (is (= ::world/invalid-fork-recovery
                (:type (ex-data
                        (failure #(sci/recover-fork!
                                   (ex-info "forged recovery" {:type ::world/fork-cleanup-failed
                                                               :recovery-world (:sci.impl/world parent)})))))))
         (is (= :live (sci/eval-string* parent ":live")))))))
