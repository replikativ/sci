(ns sci.fork-interleaving-test
  "Regressions for interleavings of forks, retirement and publications found
   in review. Each test forces its interleaving with hooks on world internals
   and latches (bounded waits on conditions), not sleeps."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.world :as world])
  (:import [java.lang.ref WeakReference]))

(defn- forkable [opts]
  (sci/init (assoc opts :runtime-mode :forkable)))

(defn- await-condition
  "Wait up to `ms` for `(pred)`; whether it became true."
  [ms pred]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/onSpinWait) (recur))))))

(defn- slot-of [ctx handle]
  (world/slot-of (:sci.impl/world ctx) handle))

(deftest an-env-watch-may-wait-for-a-fork
  ;; A watch on the context's env atom is user code: it must not run inside a
  ;; publication, where a fork it waits for could never take the gate.
  (let [ctx (forkable {})
        _ (sci/eval-string* ctx "(def x 0)")
        forked (promise)]
    (add-watch (:env ctx) ::fork-from-watch
               (fn [_ _ _ _]
                 (when-not (realized? forked)
                   (deliver forked (deref (future (sci/fork ctx)) 2000 ::timeout)))))
    (try
      (sci/eval-string* ctx "(def x 1)")
      (is (map? (deref forked 5000 ::no-watch)) "the fork the watch waited for completed")
      (finally (remove-watch (:env ctx) ::fork-from-watch)))))

(deftest first-realization-never-copies-a-dependent-root-without-its-prerequisite
  ;; Two volatiles of a still-mutable primary: b is set from a. The first fork
  ;; realizes them one after the other; a write of a and then of b between
  ;; those reads must not give the child the new b with the old a.
  (let [t1-entered (promise) t1-go (promise) t2-entered (promise)
        ctx (forkable {:namespaces
                       {'host {'t1 #(do (deliver t1-entered true) (deref t1-go 5000 :timeout))
                               't2-entered #(deliver t2-entered true)
                               'await-written (fn [v] (await-condition 1000 #(= 1 @v)))}}})
        [v0 v1] (sci/eval-string* ctx "(def vs [(volatile! 0) (volatile! 0)]) vs")
        order (keys (:descriptors @(.-registry ^sci.impl.world.DenseWorld (:sci.impl/world ctx))))
        a-index (if (< (.indexOf ^java.util.List (vec order) v0)
                       (.indexOf ^java.util.List (vec order) v1)) 0 1)
        [a b] (if (zero? a-index) [v0 v1] [v1 v0])
        b-index (- 1 a-index)
        a-slot (slot-of ctx a)
        cells-set! @#'world/cells-set!
        fork-thread (promise)
        t1 (future (sci/eval-string* ctx (str "(vswap! (vs " a-index ") (fn [_] (host/t1) 1))")))
        _ (deref t1-entered 5000 :timeout)
        t2 (future (sci/eval-string*
                    ctx (str "(vswap! (vs " b-index ") (fn [_] (host/t2-entered)"
                             " (host/await-written (vs " a-index ")) @(vs " a-index ")))")))
        _ (deref t2-entered 5000 :timeout)
        child (with-redefs [world/cells-set!
                            (fn [cells slot value]
                              (when (and (identical? (Thread/currentThread) @fork-thread)
                                         (= slot a-slot))
                                ;; a's root has been read for the child
                                (deliver t1-go true)
                                (await-condition 1000 #(= 1 @b)))
                              (cells-set! cells slot value))]
                (deref (future (deliver fork-thread (Thread/currentThread)) (sci/fork ctx))
                       10000 ::timeout))]
    (deliver t1-go true)
    (is (= 1 (deref t1 5000 ::timeout)))
    (deref t2 5000 ::timeout)
    (is (map? child))
    (when (map? child)
      (let [[ca cb] (sci/eval-string* child (str "[@(vs " a-index ") @(vs " b-index ")]"))]
        (is (<= cb ca) (str "child has b=" cb " without its prerequisite a=" ca))))))

(deftest a-mutable-primary-write-racing-the-first-fork-keeps-slot-and-root-equal
  (let [entered (promise) go (promise)
        ctx (forkable {:namespaces {'host {'entered #(deliver entered true)
                                           'wait #(deref go 5000 :timeout)}}})
        v (sci/eval-string* ctx "(def v (volatile! 0)) v")
        v-slot (slot-of ctx v)
        t1 (future (sci/eval-string* ctx "(vswap! v (fn [_] (host/entered) (host/wait) 1))"))
        _ (deref entered 5000 :timeout)
        _ (sci/fork ctx)                    ; realizes the primary meanwhile
        cells-set! @#'world/cells-set!
        t1-thread (promise)]
    (with-redefs [world/cells-set!
                  (fn [cells slot value]
                    (when (and (= slot v-slot) (= 1 value)
                               (deliver t1-thread (Thread/currentThread))
                               (identical? (Thread/currentThread) @t1-thread))
                      ;; t1 is about to publish 1: another writer commits 2 first
                      (deref (future (sci/eval-string* ctx "(vreset! v 2)")) 2000 :timeout))
                    (cells-set! cells slot value))]
      (deliver go true)
      (deref t1 5000 ::timeout))
    (is (= (sci/eval-string* ctx "@v") (sci/eval-string* (sci/fork ctx) "@v"))
        "a later fork copies the value the primary reads")))

(deftest retirement-racing-a-fork-copy-keeps-the-childs-lazy-values
  (let [ctx (forkable {})
        _ (sci/eval-string* ctx "(def xs (lazy-seq [1 2]))")
        owners @#'world/world-cache-owners
        fork-thread (promise)
        retirement (promise)]
    (let [child (with-redefs [world/world-cache-owners
                              (fn [& args]
                                (when (and (identical? (Thread/currentThread) @fork-thread)
                                           (not (realized? retirement)))
                                  ;; the cells are copied; retire the source now
                                  (let [r (future (sci/retire! ctx) :retired)]
                                    (deliver retirement r)
                                    (deref r 500 ::waiting)))
                                (apply owners args))]
                  (deref (future (deliver fork-thread (Thread/currentThread)) (sci/fork ctx))
                         10000 ::timeout))]
      (is (= :retired (deref @retirement 5000 ::timeout)))
      (is (map? child))
      (when (map? child)
        (is (= [1 2] (sci/eval-string* child "(vec xs)")))))))

(defn- collected?
  "Whether every weak reference is cleared after (repeated) GC."
  [refs]
  (let [cleared? #(every? (fn [^WeakReference r] (nil? (.get r))) refs)]
    (loop [n 0]
      (cond (cleared?) true
            (= n 20) false
            :else (do (System/gc) (await-condition 50 cleared?) (recur (inc n)))))))

(defn- cells-length [ctx]
  (.length ^java.util.concurrent.atomic.AtomicReferenceArray
           @(.-cells-holder ^sci.impl.world.DenseWorld (:sci.impl/world ctx))))

(deftest retired-heaps-do-not-accumulate
  ;; The retired world itself may stay (Var homes keep it, so access to a
  ;; retired Var is refused); its cells and caches must not.
  (let [parent (forkable {})
        _ (sci/eval-string* parent "(def shared 1)")
        children (vec (for [i (range 8)]
                        (let [child (sci/fork parent)
                              big (sci/eval-string*
                                   child (str "(def child-only-" i " (vec (range 100000))) (def shared " i ") child-only-" i))]
                          (sci/retire! child)
                          [(WeakReference. big) (cells-length child)])))]
    (is (every? zero? (map second children)) "a retired world with no evaluation in it is cleared")
    (is (collected? (map first children)) "retired heaps are garbage collected")
    (is (= 1 (sci/eval-string* parent "shared")))
    (is (= 1 (sci/eval-string* (sci/fork parent) "shared")))))

(deftest an-interned-closure-root-does-not-retain-a-retired-heap
  (let [parent (forkable {})
        child (sci/fork parent)
        big (sci/eval-string* child "(let [big (vec (range 100000))]
                                       (intern 'user 'leak (fn [] (count big)))
                                       big)")
        ;; the registry keeps the Var and its root closure, which captures
        ;; the child's context: the retired heap must still go
        heap (WeakReference. @(.-cells-holder ^sci.impl.world.DenseWorld (:sci.impl/world child)))]
    (is (= 100000 (sci/eval-string* child "(leak)")))
    (sci/retire! child)
    (is (collected? [heap]))
    (is (= 100000 (count big)))
    (is (= :parent-live (sci/eval-string* parent ":parent-live")) "the lineage registry stayed alive")))

(deftest a-var-defined-by-an-admitted-evaluation-after-retirement-is-reclaimed
  (let [entered (promise) go (promise) captured (atom nil)
        parent (forkable {:namespaces {'host {'pause #(do (deliver entered true) (deref go 5000 :timeout))
                                              'capture #(reset! captured (WeakReference. %))}}})
        child (sci/fork parent)
        t1 (future (sci/eval-string* child "(host/pause) (def late (vec (range 100000))) (host/capture late) :done"))]
    (is (true? (deref entered 5000 ::timeout)))
    (sci/retire! child)
    (deliver go true)
    (is (= :done (deref t1 5000 ::timeout)) "the admitted evaluation finished")
    (is (zero? (cells-length child)) "the last evaluation leaving the retired world cleared it")
    (is (collected? [@captured]))
    (is (= :parent-live (sci/eval-string* parent ":parent-live")))))

(deftest a-retired-var-access-errors-also-after-gc
  (let [parent (forkable {})
        v (let [child (sci/fork parent)
                v (sci/eval-string* child "(def weak-only 123) #'weak-only")]
            (sci/retire! child)
            v)]
    (dotimes [_ 5] (System/gc) (await-condition 20 (constantly false)))
    (is (re-find #"retired"
                 (str (try (deref v) (catch Exception e (ex-message e)))))
        "never a silent fallback to the raw root")
    (is (= :parent-live (sci/eval-string* parent ":parent-live")))))

(deftest loaded-libs-are-cut-with-the-environment
  (let [ctx (forkable {:load-fn (fn [{:keys [namespace]}]
                                  {:file (str namespace ".clj")
                                   :source (str "(ns " namespace ") (def loaded 1)")})})
        fork-values! @#'world/fork-values!
        fork-thread (promise)
        child (with-redefs [world/fork-values!
                            (fn [& args]
                              (when (identical? (Thread/currentThread) @fork-thread)
                                ;; after the copy section: the parent loads newlib
                                (deref (future (sci/eval-string* ctx "(require 'newlib)")) 5000 :timeout))
                              (apply fork-values! args))]
                (deref (future (deliver fork-thread (Thread/currentThread)) (sci/fork ctx))
                       10000 ::timeout))]
    (is (= [true true] (sci/eval-string* ctx "[(boolean (find-ns 'newlib)) (contains? (loaded-libs) 'newlib)]")))
    (is (map? child))
    (when (map? child)
      (is (= [false false]
             (sci/eval-string* child "[(boolean (find-ns 'newlib)) (contains? (loaded-libs) 'newlib)]"))))))

(deftest concurrent-definitions-of-one-var-never-tear-value-and-metadata
  (let [ctx (forkable {})
        _ (sci/eval-string* ctx "(def ^{:revision 0} x 0)")
        register-var! @#'world/register-var!
        t1-thread (promise)
        t2 (promise)]
    (with-redefs [world/register-var!
                  (fn [& args]
                    (let [result (apply register-var! args)]
                      (when (and (identical? (Thread/currentThread) @t1-thread)
                                 (= 1 (second args)) (not (realized? t2)))
                        ;; t1 wrote x's value; its metadata is still to come
                        (let [f (future (sci/eval-string* ctx "(def ^{:revision 2} x 2)"))]
                          (deliver t2 f)
                          (deref f 500 ::waiting)))
                      result))]
      (deref (future (deliver t1-thread (Thread/currentThread))
                     (sci/eval-string* ctx "(def ^{:revision 1} x 1)"))
             10000 ::timeout)
      (deref @t2 10000 ::timeout))
    (let [q "[x (:revision (meta #'x))]"
          [value revision] (sci/eval-string* ctx q)]
      (is (= value revision) "one definition's value with another's metadata")
      (is (= [value revision] (sci/eval-string* (sci/fork ctx) q))))))

(deftest an-untrusted-future-is-never-asked-whether-it-is-done
  ;; isDone of an arbitrary Future is host code; a fork never calls it.
  (let [called (atom 0)
        task (reify java.util.concurrent.Future
               (isDone [_] (swap! called inc) true)
               (isCancelled [_] false)
               (cancel [_ _] false)
               (get [_] :host-value)
               (get [_ _ _] :host-value))
        ctx (forkable {:namespaces {'host {'task task}}})
        _ (sci/eval-string* ctx "(def task host/task)")
        child (sci/fork ctx)]
    (is (zero? @called))
    (is (= :sci/forked-future
           (sci/eval-string* child "(try @task (catch Exception e (:type (ex-data e))))")))
    (testing "trusted completed futures are still shared"
      (let [cf (java.util.concurrent.CompletableFuture/completedFuture :done)
            ctx (forkable {:namespaces {'host {'cf cf}}})
            _ (sci/eval-string* ctx "(def cf host/cf)")]
        (is (identical? cf (sci/eval-string* (sci/fork ctx) "cf")))))))


(deftest a-child-volatile-keeps-no-value-in-its-shared-root
  ;; The registry keeps a volatile created in a fork for the lineage's
  ;; lifetime; its value lives only in the child's slots.
  (let [captured (atom nil)
        parent (forkable {:namespaces {'host {'make #(let [o (Object.)]
                                                        (reset! captured (WeakReference. o))
                                                        o)}}})
        child (sci/fork parent)
        v (sci/eval-string* child "(let [big (host/make)]
                                     (def v (volatile! (fn [] big)))
                                     v)")]
    (is (some? (sci/eval-string* child "(@v)")))
    (is (nil? @v) "host deref outside any world reads the empty shared root")
    (sci/retire! child)
    (is (collected? [@captured]) "the initial value and what it captures are collected")
    (is (= :parent-live (sci/eval-string* parent ":parent-live")))))

(deftest a-core-future-with-an-untrusted-delegate-is-never-asked-whether-it-is-done
  ;; clojure.core/future delegates isDone to what the send-off executor's
  ;; submit returned; a custom executor can return any Future.
  (let [called (atom 0)
        original clojure.lang.Agent/soloExecutor
        executor (proxy [java.util.concurrent.AbstractExecutorService] []
                   (execute [^Runnable r] (.start (Thread. r)))
                   (submit [task]
                     ;; on its own thread: the task resets its thread's binding frame
                     (let [t (proxy [java.util.concurrent.FutureTask] [^java.util.concurrent.Callable task]
                               (isDone [] (swap! called inc) true))]
                       (.start (Thread. ^Runnable t))
                       t))
                   (shutdown [])
                   (shutdownNow [] [])
                   (isShutdown [] false)
                   (isTerminated [] false)
                   (awaitTermination [_ _] true))
        task (try (set-agent-send-off-executor! executor)
                  (future :host-value)
                  (finally (set-agent-send-off-executor! original)))
        ctx (forkable {:namespaces {'host {'task task}}})
        _ (sci/eval-string* ctx "(def task host/task)")
        child (sci/fork ctx)]
    (is (zero? @called) "the delegate's isDone was not called")
    (is (= :sci/forked-future
           (sci/eval-string* child "(try @task (catch Exception e (:type (ex-data e))))")))
    (testing "a core future over a FutureTask is still trusted"
      (let [done (future :done)
            _ (deref done 5000 :timeout)
            ctx (forkable {:namespaces {'host {'done done}}})
            _ (sci/eval-string* ctx "(def done host/done)")]
        (is (identical? done (sci/eval-string* (sci/fork ctx) "done")))))))

(deftest a-var-watch-may-wait-for-a-redefinition
  ;; Standard contexts keep their behaviour (no SCI lock around a def, whose
  ;; watches run at once); in a forkable one watches run after the def's
  ;; stripe and publication are released.
  (doseq [opts [{} {:runtime-mode :forkable}]]
  (let [ctx (sci/init opts)
        _ (sci/eval-string* ctx "(def x 0)")
        redefined (promise)
        x (sci/eval-string* ctx "#'x")]
    (add-watch x ::redefine
               (fn [_ _ _ n]
                 (when (= 1 n)
                   (let [done (promise)]
                     (.start (Thread. ^Runnable #(deliver done (sci/eval-string* ctx "(def x 2)"))))
                     (deliver redefined (deref done 2000 ::timeout))))))
    (sci/eval-string* ctx "(def x 1)")
    (is (not= ::timeout (deref redefined 5000 ::no-watch)) (pr-str opts))
    (is (= 2 (sci/eval-string* ctx "x"))))))

(deftest caches-created-after-retirement-by-an-admitted-evaluation-are-cleared
  (let [captured (atom nil) held (atom nil) self (atom nil)
        parent (forkable {:namespaces
                          {'host {'retire! #(sci/retire! @self)
                                  'make #(let [o (Object.)] (reset! captured (WeakReference. o)) o)
                                  'hold #(reset! held %)}}})
        child (sci/fork parent)]
    (reset! self child)
    (is (= :done (sci/eval-string*
                  child "(def child-var 1)
                         (host/retire!)
                         (let [o (host/make)] (host/hold (lazy-seq (list o))))
                         :done")))
    (is (zero? (cells-length child)))
    (is (collected? [@captured]) "the owner's entry for the retired world was released")
    (is (some? @held))
    (is (= :parent-live (sci/eval-string* parent ":parent-live")))))

(deftest a-related-world-without-the-vars-slot-consults-its-home
  (let [p (forkable {})
        c (sci/fork p)
        v (sci/eval-string* c "(intern 'user 'child-only 123)")]
    (is (= 123 (sci/call-with-context p #(deref v))) "the open home's value")
    (sci/retire! c)
    (is (re-find #"retired" (str (try (sci/call-with-context p #(deref v))
                                      (catch Exception e (ex-message e))))))
    (let [sibling (sci/fork p)]
      (is (re-find #"retired" (str (try (sci/call-with-context sibling #(deref v))
                                        (catch Exception e (ex-message e)))))))))

(deftest compiled-var-reads-in-a-related-world-consult-the-home
  ;; A fn compiled in child c reads x through its slot (VarNode) and calls
  ;; target through getRootAt. Run in a sibling that has no realization of
  ;; them, both answer from their home world, or refuse once it is retired.
  (let [p (forkable {})
        older (sci/fork p)
        c (sci/fork p)
        [f g] (sci/eval-string* c "(def x 123) (def target (fn [] 456)) [(fn [] x) (fn [] (target))]")
        newer (sci/fork p)
        descendant (sci/fork c)
        attempt (fn [ctx h] (try (sci/call-with-context ctx h)
                                 (catch Exception e (if (re-find #"retired" (str (ex-message e)))
                                                      :retired
                                                      (str (class e) ": " (ex-message e))))))]
    (doseq [[label ctx] [[:older older] [:newer newer] [:descendant descendant]]]
      (is (= 123 (attempt ctx f)) (str label " value read"))
      (is (= 456 (attempt ctx g)) (str label " call through the Var")))
    (sci/retire! c)
    (doseq [[label ctx] [[:older older] [:newer newer]]]
      (is (= :retired (attempt ctx f)) (str label " value read after retire"))
      (is (= :retired (attempt ctx g)) (str label " call after retire")))
    (is (= [123 456] [(attempt descendant f) (attempt descendant g)])
        "a descendant keeps its own copies")))

;; The built-in interface methods of a SCI type live in its Type's world data
;; (babashka/sci#1132). For a type defined in a child, a world without that
;; data (the parent, an older sibling) and the host read its home world, as
;; for Vars; a descendant has its own copy.
(deftest a-child-types-interface-methods-consult-its-home
  (let [p (forkable {})
        older (sci/fork p)
        c (sci/fork p)
        d (sci/eval-string*
           c "(deftype D [x]
                clojure.lang.IDeref (deref [_] x)
                clojure.lang.IFn (invoke [_ a] (+ x a))
                Object (toString [_] (str \"D:\" x))
                       (hashCode [_] (int x))
                       (equals [_ o] (and (instance? D o) (= x (.-x o)))))
              (->D 42)")
        other (sci/eval-string* c "(->D 42)")
        descendant (sci/fork c)
        newer (sci/fork p)
        use-it "(fn [x] [@x (x 1) (str x) (hash x) (= x x)])"
        fns (into {} (for [[label ctx] [[:parent p] [:older older] [:newer newer]
                                        [:descendant descendant]]]
                       [label (sci/eval-string* ctx use-it)]))
        attempt (fn [h] (try (h)
                             (catch Exception e
                               (if (re-find #"retired" (str (ex-message e)))
                                 :retired
                                 (str (class e) ": " (ex-message e))))))
        host (fn [] [(.hashCode ^Object d) (.equals ^Object d other) (str d) (d 1)
                     (.get (doto (java.util.HashMap.) (.put d :v)) other)])
        expected [42 43 "D:42" 42 true]]
    (doseq [[label f] fns]
      (is (= expected (attempt #(f d))) (str label " while the home is open")))
    (is (= [42 true "D:42" 43 :v] (attempt host)) "host while the home is open")
    (sci/retire! c)
    (doseq [label [:parent :older :newer]]
      (is (= :retired (attempt #((get fns label) d))) (str label " after retire")))
    (is (= :retired (attempt host)) "host after retire")
    (is (= expected (attempt #((:descendant fns) d))) "a descendant keeps its own copy")
    (is (= "user.D" (str (sci/eval-string* descendant "D")))
        "printing the type needs no world")))

;; A type's methods come from the world's own definition, else from its home
;; (the world that evaluated the deftype), as a Var's value does. A deftype
;; analysed in the parent (inside a fn) and evaluated in a child has the
;; child as home. Host calls follow the home: once it is retired they are
;; refused, also on copies a fork made or instances nested in collections,
;; while code in a world with its own definition keeps working.
(defn- attempt [h]
  (try (h)
       (catch Exception e
         (if (re-find #"retired" (str (ex-message e)))
           :retired
           (str (class e) ": " (ex-message e))))))

(defn- host-use [x y]
  [(.hashCode ^Object x) (.equals ^Object x y)
   (.get (doto (java.util.HashMap.) (.put x :v)) y)])

(deftest type-methods-follow-the-defining-world
  (let [p (forkable {})
        _ (sci/eval-string*
           p "(defn mk [] (deftype N [x] clojure.lang.IDeref (deref [_] x)
                                      Object (hashCode [_] (int x))
                                             (equals [_ o] (= x (.-x o))))
                          (->N 5))")
        c (sci/fork p)
        n (sci/eval-string* c "(def kept (mk)) kept")
        g (sci/fork c)
        kept-in-g (sci/eval-string* g "kept")]
    (is (= 5 ((sci/eval-string* p "(fn [x] @x)") n)) "the parent")
    (is (= [5 true :v] (attempt #(host-use n (sci/eval-string* c "(mk)")))) "the host")
    (sci/retire! c)
    (is (= :retired (attempt #(host-use n n))) "the host after the home is retired")
    (is (= :retired (attempt #(host-use kept-in-g kept-in-g)))
        "a fork's copy: host calls follow the home")
    (is (= 5 (sci/eval-string* g "@kept")) "a world with its own definition"))
  (testing "evaluated in both: the first definer is the home"
    (let [p (forkable {})
          _ (sci/eval-string*
             p "(defn mk [] (deftype N [x] Object (hashCode [_] (int x))) (->N 5))")
          c (sci/fork p)
          from-p (sci/eval-string* p "(mk)")
          from-c (sci/eval-string* c "(mk)")]
      (sci/retire! c)
      (is (= [5 5] [(.hashCode ^Object from-p) (.hashCode ^Object from-c)])
          "the home (the parent) is open")
      (sci/retire! p)
      (is (= :retired (attempt #(.hashCode ^Object from-c))))))
  (testing "a retired primary"
    (let [p (forkable {})
          d (sci/eval-string* p "(deftype D [x] Object (hashCode [_] 7) (toString [_] \"d\")) (->D 1)")
          r (sci/eval-string* p "(defrecord R [a]) (->R 1)")
          h (hash r)]
      (sci/retire! p)
      (is (= :retired (attempt #(.hashCode ^Object d))))
      (is (re-find #"retired SCI world" (pr-str d)))
      (is (= ["#user.R{:a 1}" h true] [(pr-str r) (hash r) (= r r)]))
      (require 'sci.pprint)
      (is (re-find #"\{:a 1\}" (with-out-str ((requiring-resolve 'clojure.pprint/pprint) r)))
          "sci.pprint prints a retired record's fields"))))

(deftest instances-nested-in-a-forks-values-follow-the-home
  (let [p (forkable {})
        _ (sci/eval-string*
           p "(defrecord R [a] Object (toString [_] \"R!\"))
              (deftype T [x] Object (toString [_] \"T!\"))
              (def nested [(->R 1) (->T 1)]) (def a (atom {:r (->R 1)}))")
        c (sci/fork p)
        [[r t] av] (sci/eval-string* c "[nested @a]")]
    (sci/retire! p)
    (is (= ["R!" "T!" "R!"]
           (sci/eval-string* c "[(str (first nested)) (str (second nested)) (str (:r @a))]"))
        "the child has its own definitions")
    (is (= [:retired :retired :retired] [(attempt #(str r)) (attempt #(str t)) (attempt #(str (:r av)))])
        "host calls follow the retired home")
    (is (= "#user.R{:a 1}" (pr-str r)) "printing falls back")))

(deftest an-instance-of-a-parents-type-does-not-retain-the-child
  (let [p (forkable {})
        _ (sci/eval-string* p "(defrecord R [a]) (deftype T [x])")
        make (fn [src]
               ;; (no def in the child: the lineage registry keeps the Vars
               ;; it defines, and they pin their home)
               (let [c (sci/fork p)]
                 [(WeakReference. (:sci.impl/world c)) (sci/eval-string* c src)]))
        [wr r] (make "(->R 1)")
        [wt t] (make "(->T 1)")
        [wv v] (make "[(->R 1) (->T 2)]")]
    (is (collected? [wr wt wv]) "the child worlds are collected")
    (is (every? some? [r t v]) "while their instances are kept")))

(deftest a-host-read-racing-retirement-never-sees-a-cleared-array
  (let [p (forkable {})
        c (sci/fork p)
        v (sci/eval-string* c "(def x 123) #'x")
        cw (:sci.impl/world c)
        current-cells @#'world/current-cells
        reader (promise)
        retired (promise)]
    (with-redefs [world/current-cells
                  (fn [w]
                    (let [cells (current-cells w)]
                      (when (and (identical? w cw)
                                 (identical? (Thread/currentThread) @reader)
                                 (not (realized? retired)))
                        ;; the reader holds this array: retire c now
                        (deliver retired (deref (future (sci/retire! c) :retired) 1000 ::waiting)))
                      cells))]
      (let [result (deref (future (deliver reader (Thread/currentThread))
                                  (try @v (catch Exception e (if (re-find #"retired" (str (ex-message e)))
                                                               :retired
                                                               (str (class e))))))
                          5000 ::timeout)]
        (is (= :retired @retired))
        (is (contains? #{123 :retired} result) (str result))))))

(deftype FailingOnceOwner [cache failures calls]
  world/IForkableWorldCache
  (-fork-world-cache! [_ source target]
    (when (contains? @cache source) (swap! cache assoc target (get @cache source))))
  world/IWorldCache
  (-retire-world-cache! [_ selected]
    (swap! calls inc)
    (if (pos? @failures)
      (do (swap! failures dec) (throw (ex-info "cleanup failed once" {})))
      (swap! cache dissoc selected))))

(deftest a-failed-final-cleanup-is-retried-by-retire
  (let [self (atom nil) owner (atom nil)
        parent (forkable {:namespaces
                          {'host {'retire! #(sci/retire! @self)
                                  'register! #(let [w (world/selected-world)
                                                    o (FailingOnceOwner. (atom {w :cached}) (atom 1) (atom 0))]
                                                (reset! owner o)
                                                (world/register-world-cache! o)
                                                nil)}}})
        child (sci/fork parent)
        cw (:sci.impl/world child)]
    (reset! self child)
    (try (sci/eval-string* child "(host/retire!) (host/register!) :done")
         (catch Exception _ nil))
    (is (= 1 @(.-calls ^FailingOnceOwner @owner)) "the final clear attempted the late owner")
    (is (contains? @(.-cache ^FailingOnceOwner @owner) cw))
    (is (nil? (sci/retire! child)))
    (is (= 2 @(.-calls ^FailingOnceOwner @owner)) "a retried retire! resumes the cleanup")
    (is (not (contains? @(.-cache ^FailingOnceOwner @owner) cw)))))
