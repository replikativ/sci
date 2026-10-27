(ns sci.impl.world
  "Dense, fork-local runtime state for SCI contexts.

  Stable handles are assigned integer slots once per context lineage. A world
  stores slot values densely and a frozen fork copies that array while the
  source world is quiescent. Mutable primitives CAS only their own slot, so
  unrelated atoms never contend on a shared world root."
  (:require [sci.ctx-store :as store]
            [sci.impl.execution :as execution])
  #?@(:cljd []
      :clj [(:import [java.lang.ref WeakReference]
                     [java.util IdentityHashMap WeakHashMap]
                     [java.util.concurrent.atomic AtomicReferenceArray]
                     [java.util.concurrent.locks ReentrantReadWriteLock])]))

(def absent #?(:cljd (Object.) :clj (Object.) :cljs (js/Object.)))

(deftype DenseWorld [cells-holder registry gate resize-gate persistent? open? evaluation-depth])

(defprotocol IForkableWorldCache
  (-fork-world-cache! [owner source target]
    "Copy an existing source realization into the unpublished target without
     invoking deferred computation. Missing source state remains missing."))

(defprotocol IWorldCache
  (-retire-world-cache! [owner world]
    "Release this managed owner's cache for one quiescent retired world.
     This internal hook only removes private cache entries: no user callbacks
     or I/O. It must be idempotent so failed retirement can be retried."))

(defn assert-open! [^DenseWorld world]
  (when-not @(.-open? world)
    (throw (ex-info "SCI world has been retired." {:type ::retired-world})))
  world)
(deftype ReadWorld [world primary?])

(defprotocol IWorldTracked
  (-world-tracked? [handle])
  (-world-home-ctx [handle])
  (-mark-world-tracked! [handle home-ctx]))

(defn read-world [world primary?]
  (ReadWorld. world primary?))

(defn- new-cells [n]
  #?(:cljd
     (#/(List/filled dynamic) n absent)
     :clj
     (let [a (AtomicReferenceArray. (int n))]
       (dotimes [i n] (.set a i absent))
       a)
     :cljs
     (let [a (object-array n)]
       (dotimes [i n] (aset a i absent))
       a)))

(defn- cells-length [cells]
  #?(:cljd (.-length ^List cells)
     :clj (.length ^AtomicReferenceArray cells)
     :cljs (alength cells)))

(defn- cells-get [cells slot]
  #?(:cljd (aget ^List cells slot)
     :clj (.get ^AtomicReferenceArray cells (int slot))
     :cljs (aget cells slot)))

(defn- cells-set! [cells slot value]
  #?(:cljd (aset ^List cells slot value)
     :clj (.set ^AtomicReferenceArray cells (int slot) value)
     :cljs (aset cells slot value))
  value)

(defn- cells-cas! [cells slot old new]
  #?(:cljd
     (if (identical? old (cells-get cells slot))
       (do (cells-set! cells slot new) true)
       false)
     :clj (.compareAndSet ^AtomicReferenceArray cells (int slot) old new)
     :cljs
     (if (identical? old (cells-get cells slot))
       (do (cells-set! cells slot new) true)
       false)))

(defn- current-cells [^DenseWorld world]
  @(.-cells-holder world))

(defn- with-cells-read-lock [^DenseWorld world f]
  #?(:cljd (f)
     :clj (let [lock (.readLock ^ReentrantReadWriteLock (.-resize-gate world))]
            (.lock lock)
            (try (f) (finally (.unlock lock))))
     :cljs (f)))

(defn- with-cells-write-lock [^DenseWorld world f]
  #?(:cljd (f)
     :clj (let [lock (.writeLock ^ReentrantReadWriteLock (.-resize-gate world))]
            (.lock lock)
            (try (f) (finally (.unlock lock))))
     :cljs (f)))

(defn- with-coordination [coordination f]
  #?(:cljd (f)
     :clj (if coordination (locking coordination (f)) (f))
     :cljs (f)))

(defn- with-registry-lock [registry f]
  #?(:cljd (f)
     :clj (locking registry (f))
     :cljs (f)))

(defn- ensure-capacity! [^DenseWorld world required]
  (let [holder (.-cells-holder world)]
    (when (> required (cells-length @holder))
      (with-cells-write-lock
        world
        (fn []
          (let [old @holder
                old-n (cells-length old)]
            (when (> required old-n)
              (let [new-n (loop [n (max 16 old-n)]
                            (if (>= n required) n (recur (* 2 n))))
                    new (new-cells new-n)]
                (dotimes [i old-n]
                  (cells-set! new i (cells-get old i)))
                (vreset! holder new))))))))
  world)

(defn- new-world-cache-index []
  #?(:clj (WeakHashMap.)
     :cljs
     (when (and (exists? js/WeakRef) (exists? js/WeakMap))
       (let [refs (js/Set.)]
         {:refs refs
          :by-owner (js/WeakMap.)
          :registrations (volatile! 0)
          :finalizer (when (exists? js/FinalizationRegistry)
                       (js/FinalizationRegistry. (fn [ref] (.delete refs ref))))}))
     :cljd nil))

(defn new-world []
  (DenseWorld.
   (volatile! (new-cells 64))
   (atom {:next-slot 0 :descriptors {} :managed-descriptors []
          :world-caches (new-world-cache-index)})
   #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
   #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
   (volatile! false)
   (volatile! true)
   (volatile! 0)))

(defn active-world-state []
  #?(:cljd (execution/active-world)
     :clj (let [state (.get ^ThreadLocal execution/current)]
            (aget ^objects state execution/active-world-index))
     :cljs (execution/active-world)))

(defn- install-active-world! [state active]
  (execution/set-active-world! state active)
  (if (and active (not (.-primary? ^ReadWorld active)))
    (let [^DenseWorld world (.-world ^ReadWorld active)]
      (execution/set-active-cells-holder! state (.-cells-holder world))
      (execution/set-active-registry! state (.-registry world)))
    (do
      (execution/set-active-cells-holder! state nil)
      (execution/set-active-registry! state nil)))
  active)

(defn- call-with-active-world [ctx f]
  (let [state #?(:cljd (execution/current-state)
                 :clj (.get ^ThreadLocal execution/current)
                 :cljs (execution/current-state))
        previous (execution/active-world state)
        previous-scope (execution/binding-scope state)
        active (:sci.impl/read-world ctx)]
    (install-active-world! state active)
    (execution/set-binding-scope! state (:sci.impl/world ctx))
    (execution/refresh-active-bindings! state)
    (try
      (f)
      (finally
        (install-active-world! state previous)
        (execution/set-binding-scope! state previous-scope)
        (execution/refresh-active-bindings! state)))))

(defn selected-world
  "The actual selected world, including the target selected during copying."
  []
  (some-> (active-world-state) (.-world)))

(defn register-world-cache!
  "Register an identity-based internal owner of fork-local realizations. The
   lineage index retains the owner weakly and does not allocate dense slots.
   Owners must implement both cache protocols and must not use content-based
   hashing/equality (in particular, the owner is not a sequence view)."
  [owner]
  (let [^DenseWorld world (or (selected-world) (:sci.impl/world store/*ctx*))]
    (when-not world
      (throw (ex-info "Managed caches must be registered inside an SCI context."
                      {:type ::missing-world})))
    (assert-open! world)
    (when-not (and (satisfies? IWorldCache owner)
                   (satisfies? IForkableWorldCache owner))
      (throw (ex-info "Managed cache owner must support fork and retirement."
                      {:type ::invalid-world-cache-owner})))
    (let [index (:world-caches @(.-registry world))]
      (when-not index
        (throw (ex-info "This host has no weak cache-owner support."
                        {:type ::weak-cache-owners-unavailable})))
      #?(:clj
         (locking index (.put ^WeakHashMap index owner true))
         :cljs
         (when-not (.has (:by-owner index) owner)
           (let [ref (js/WeakRef. owner)
                 finalizer (:finalizer index)]
             (.set (:by-owner index) owner ref)
             (.add (:refs index) ref)
             (if finalizer
               (.register finalizer owner ref ref)
               ;; Without finalization notifications, incremental registration
               ;; periodically prunes dead refs. No owner is strongly retained.
               (when (= 64 (vswap! (:registrations index) inc))
                 (vreset! (:registrations index) 0)
                 (.forEach (:refs index)
                           (fn [entry]
                             (when-not (.deref entry)
                               (.delete (:refs index) entry))))))))
         :cljd nil)))
  owner)

(defn world-cache-owners
  "An eager strong snapshot of currently live cache owners in a lineage.
   Inspection also removes dead weak entries. Closed worlds retain their
   registry until cleanup succeeds, so retirement may call this function."
  ([]
   (if-let [world (or (selected-world) (:sci.impl/world store/*ctx*))]
     (world-cache-owners world)
     []))
  ([^DenseWorld world]
   (let [index (:world-caches @(.-registry world))]
     #?(:clj
        (if index
          (locking index (vec (.keySet ^WeakHashMap index)))
          [])
        :cljs
        (if index
          (let [owners (volatile! [])]
            (.forEach (:refs index)
                      (fn [ref]
                        (if-let [owner (.deref ref)]
                          (vswap! owners conj owner)
                          (do (.delete (:refs index) ref)
                              (when-let [finalizer (:finalizer index)]
                                (.unregister finalizer ref))))))
            @owners)
          [])
        :cljd []))))

(defn- with-counted-world-access [^DenseWorld world f]
  (assert-open! world)
  (vswap! (.-evaluation-depth world) inc)
  (try
    (f)
    (finally (vswap! (.-evaluation-depth world) dec))))

(defn- with-world-access [^DenseWorld world f]
  ;; An active related evaluation already owns its read permit. Host access
  ;; still needs that permit so retirement cannot clear slots underneath it.
  (if (identical? world (selected-world))
    (do (assert-open! world) (f))
    #?(:clj
       (let [lock (.readLock ^ReentrantReadWriteLock (.-gate world))]
         (.lock lock)
         (try
           (assert-open! world)
           (f)
           (finally (.unlock lock))))
       :default (with-counted-world-access world f))))

(defn- assert-quiescent! [^DenseWorld world operation]
  (when #?(:clj (let [^ReentrantReadWriteLock gate (.-gate world)]
                  (or (pos? (.getReadHoldCount gate))
                      (.isWriteLockedByCurrentThread gate)))
           :default (pos? @(.-evaluation-depth world)))
    (throw (ex-info (str "Cannot " (name operation)
                         " a SCI world from inside its active evaluation or copy; suspend it first.")
                    {:type ::active-world :operation operation}))))

(defn with-active-world
  "Run `f` with the context's world installed. On the JVM evaluations take a
  shared gate permit; a fork takes the exclusive permit and therefore observes
  a quiescent source world without adding locks to individual reads."
  [ctx f]
  (if-let [world (:sci.impl/world ctx)]
    #?(:cljd
       (with-counted-world-access world #(call-with-active-world ctx f))
       :clj
       (let [^DenseWorld world world
             lock (.readLock ^ReentrantReadWriteLock (.-gate world))]
         (.lock lock)
         (try
           (assert-open! world)
           (call-with-active-world ctx f)
           (finally (.unlock lock))))
       :cljs
       (with-counted-world-access world #(call-with-active-world ctx f)))
    (f)))

(defn call-with-context
  "Run `f` with `ctx` installed as both SCI's dynamic context and active world.
  This is the host invocation boundary for interpreted values that outlive the
  top-level evaluation which produced them."
  [ctx f]
  (let [active (active-world-state)]
    (if (and (identical? store/*ctx* ctx)
             active
             (identical? (:sci.impl/world ctx)
                         (.-world ^ReadWorld active)))
      (f)
      (store/with-ctx ctx
        (with-active-world ctx f)))))

(defn current-world []
  (:sci.impl/world store/*ctx*))

(defn binding-scope
  "Return the currently executing world for dynamic-binding isolation. Nil
  denotes a host binding established outside managed SCI evaluation."
  []
  (execution/binding-scope))

(defn- descriptor [^DenseWorld world handle]
  (get-in @(.-registry world) [:descriptors handle]))

(defn slot-of
  "Return the value slot assigned to `handle`, or nil when it is not registered
  in this lineage. Intended for analyzer-time resolution."
  [world handle]
  (:value-slot (descriptor world handle)))

(defn- present-slot? [^DenseWorld world slot]
  (and (some? slot)
       (< slot (cells-length (current-cells world)))
       (not (identical? absent (cells-get (current-cells world) slot)))))

(defn registered? [world handle]
  (when-let [desc (descriptor world handle)]
    (present-slot? world (:value-slot desc))))

(defn primary-world? []
  (let [active (active-world-state)]
    (if active
      (.-primary? ^ReadWorld active)
      (true? (:sci.impl/primary? store/*ctx*)))))

(defn mutable-primary-world? []
  (let [active (active-world-state)]
    (and active
         (.-primary? ^ReadWorld active)
         (not @(.-persistent? ^DenseWorld (.-world ^ReadWorld active))))))

(defn tracked? [handle]
  (when-let [^ReadWorld active (active-world-state)]
    (registered? (.-world active) handle)))

(defn- selected-var-ctx [handle]
  (let [active store/*ctx*
        active-world (:sci.impl/world active)]
    (or (when (and active-world (descriptor active-world handle)) active)
        (-world-home-ctx handle))))

(defn call-with-var-context
  "Run `f` in the active related world for `handle`, or in the Var's stable
  home world when invoked directly by the host. Shared built-in Vars have no
  home and retain their direct compatibility behavior."
  [handle f]
  (if-let [ctx (selected-var-ctx handle)]
    (call-with-context ctx f)
    (f)))

(defn- slot-value [^DenseWorld world slot fallback]
  (with-world-access
   world
   #(if (nil? slot)
      fallback
      (let [cells (current-cells world)]
        (if (>= slot (cells-length cells))
          fallback
          (let [v (cells-get cells slot)]
            (if (identical? absent v) fallback v)))))))

(defn value [handle fallback]
  (if-let [^ReadWorld active (active-world-state)]
    (let [world (.-world active)]
      (slot-value world (:value-slot (descriptor world handle)) fallback))
    fallback))

(defn var-value-at
  "Read a Var through a slot already resolved by the analyzer."
  [slot fallback]
  (if-let [holder (execution/active-cells-holder)]
    (let [cells @holder]
      (if (>= slot (cells-length cells))
        fallback
        (let [v (cells-get cells slot)]
          (if (identical? absent v) fallback v))))
    fallback))

(defn var-value
  "Primary contexts retain their direct Var realization. Descendants resolve
  the stable Var handle through its dense lineage slot."
  [handle fallback]
  (let [ctx (selected-var-ctx handle)
        active (:sci.impl/read-world ctx)]
    (if (nil? active)
      fallback
      (let [world (.-world ^ReadWorld active)]
        (with-world-access
         world
         #(if (.-primary? ^ReadWorld active)
            fallback
            (slot-value world (:value-slot (descriptor world handle)) fallback)))))))

(defn var-meta [handle fallback]
  (let [ctx (selected-var-ctx handle)
        active (:sci.impl/read-world ctx)]
    (if (nil? active)
      fallback
      (let [world (.-world ^ReadWorld active)]
        (with-world-access
         world
         #(if (.-primary? ^ReadWorld active)
            fallback
            (slot-value world (:meta-slot (descriptor world handle)) fallback)))))))

(defn namespace-meta [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (.-primary? ^ReadWorld active))
      fallback
      (let [world (.-world ^ReadWorld active)]
        (slot-value world (:value-slot (descriptor world handle)) fallback)))))

(defn type-data [handle fallback]
  (let [active (active-world-state)]
    (if (or (nil? active) (.-primary? ^ReadWorld active))
      fallback
      (let [world (.-world ^ReadWorld active)]
        (slot-value world (:value-slot (descriptor world handle)) fallback)))))

(defn var-watches [handle fallback]
  (if-let [^ReadWorld active (:sci.impl/read-world (selected-var-ctx handle))]
    (let [world (.-world active)
          {:keys [watch-slot watch-fallback]} (descriptor world handle)]
      (if watch-slot
        (slot-value world watch-slot watch-fallback)
        fallback))
    fallback))

(defn- allocate-descriptor!
  [^DenseWorld world handle kind host-fork? value-fork-fn]
  (assert-open! world)
  (let [registry (.-registry world)]
    (with-registry-lock
      registry
      (fn []
        (if-let [desc (get-in @registry [:descriptors handle])]
          (if (and value-fork-fn (nil? (:value-fork-fn desc)))
            (let [desc (assoc desc :value-fork-fn value-fork-fn)]
              (swap! registry assoc-in [:descriptors handle] desc)
              desc)
            desc)
          (let [start (:next-slot @registry)
                width (if (= :var kind) 2 1)
                desc (cond-> {:kind kind
                              :value-slot start
                              :host-fork? host-fork?}
                       value-fork-fn (assoc :value-fork-fn value-fork-fn)
                       (= :var kind) (assoc :meta-slot (inc start)))]
            (swap! registry
                   (fn [r]
                     (-> r
                         (assoc :next-slot (+ start width))
                         (assoc-in [:descriptors handle] desc))))
            desc))))))

(defn register-managed!
  "Allocate dense slots whose owner carries the returned descriptor directly.
  Managed descriptors contain no owner reference, so an unreachable SCI ref is
  not retained by the lineage registry. `fork-indexes` names values that need
  host `Forkable`/`:fork-fn` processing when a child world is copied."
  [kind values fork-indexes]
  (let [^DenseWorld world (or (selected-world) (current-world))]
    (when-not world
      (throw (ex-info "Managed SCI state must be created inside an SCI context."
                      {:kind kind})))
    (assert-open! world)
    (let [registry (.-registry world)
          desc (with-registry-lock
                 registry
                 (fn []
                   (let [start (:next-slot @registry)
                         slots (mapv #(+ start %) (range (count values)))
                         fork-slots (mapv #(nth slots %) fork-indexes)
                         managed-index (count (:managed-descriptors @registry))
                         desc {:kind kind
                               :managed-index managed-index
                               :slots slots
                               :fork-slots fork-slots
                               :owner-ref nil}]
                     (swap! registry
                            (fn [r]
                              (-> r
                                  (assoc :next-slot (+ start (count values)))
                                  (update :managed-descriptors conj desc))))
                     desc)))]
      (ensure-capacity! world (:next-slot @registry))
      (with-cells-read-lock
        world
        #(doseq [[slot value] (map vector (:slots desc) values)]
           (cells-set! (current-cells world) slot value)))
      (assoc desc :home world :registry registry))))

(defn attach-managed-owner!
  "Attach weak ownership after constructing a self-described handle. The
  owner is never retained strongly by the lineage registry."
  [registry managed-index owner]
  (let [owner-ref #?(:cljd nil
                     :clj (WeakReference. owner)
                     :cljs (when (exists? js/WeakRef) (js/WeakRef. owner)))]
    (when owner-ref
      (with-registry-lock
        registry
        #(swap! registry assoc-in
                [:managed-descriptors managed-index :owner-ref]
                owner-ref)))
    owner))

(defn- live-managed-owner? [owner-ref]
  (or (nil? owner-ref)
      #?(:cljd true
         :clj (some? (.get ^WeakReference owner-ref))
         :cljs (some? (.deref owner-ref)))))

(defn- retirement-cache-owners [^DenseWorld world]
  (let [legacy (keep (fn [{:keys [owner-ref]}]
                       #?(:cljd nil
                          :clj (when owner-ref (.get ^WeakReference owner-ref))
                          :cljs (when owner-ref (.deref owner-ref))))
                     (:managed-descriptors @(.-registry world)))
        seen #?(:clj (IdentityHashMap.) :cljs (js/Set.) :cljd nil)]
    (into []
          (filter (fn [owner]
                    (and (satisfies? IWorldCache owner)
                         #?(:clj (when-not (.containsKey ^IdentityHashMap seen owner)
                                   (.put ^IdentityHashMap seen owner true)
                                   true)
                            :cljs (when-not (.has seen owner)
                                    (.add seen owner)
                                    true)
                            :cljd true))))
          (concat legacy (world-cache-owners world)))))

(defn retire-world!
  "Retire one interpreter world at a quiescent boundary. New evaluations and
   forks are refused. Managed cache owners release their world-specific state;
   this does not close foreign host resources or retire sibling worlds. Safe
   to repeat after partial cleanup; keep the context until retirement succeeds."
  [^DenseWorld world]
  (assert-quiescent! world :retire)
  (let [release! (fn []
                   ;; Empty cells mark completed retirement. Failed cache
                   ;; cleanup leaves cells intact and the world closed, so a
                   ;; retry can finish cleanup without permitting evaluation.
                   (when (or @(.-open? world)
                             (pos? (cells-length (current-cells world))))
                     (vreset! (.-open? world) false)
                     (doseq [owner (retirement-cache-owners world)]
                       (-retire-world-cache! owner world))
                     (vreset! (.-cells-holder world) (new-cells 0)))
                   nil)]
    #?(:clj
       (let [lock (.writeLock ^ReentrantReadWriteLock (.-gate world))]
         (.lock lock)
         (try (release!) (finally (.unlock lock))))
       :default (release!))))

(defn- fork-cleanup-failure [error cleanup-error target previous]
  (ex-info "SCI fork failed and its unpublished world requires cleanup recovery."
           (cond-> {:type ::fork-cleanup-failed
                    :fork-error error
                    :cleanup-error cleanup-error
                    :recovery-world target}
             previous (assoc :previous-recovery-failure previous))
           error))

(defn fork-recovery?
  "Whether a failure retains an unpublished fork target for cleanup retry."
  [error]
  (let [{:keys [type recovery-world]} (ex-data error)]
    (boolean (and (= ::fork-cleanup-failed type)
                  (instance? DenseWorld recovery-world)
                  (not @(.-open? ^DenseWorld recovery-world))))))

(defn recover-failed-fork!
  "Retry cleanup of the unpublished target retained by a failed SCI fork.
   Never re-executes the fork or changes its source. A subsequent cleanup
   failure retains the same target and original failure for another retry."
  [error]
  (let [{:keys [fork-error recovery-world]} (ex-data error)]
    (when-not (fork-recovery? error)
      (throw (ex-info "Expected an SCI fork cleanup recovery failure."
                      {:type ::invalid-fork-recovery})))
    (try
      (retire-world! recovery-world)
      (catch #?(:clj Throwable :cljs :default :cljd Object) cleanup-error
        (throw (fork-cleanup-failure fork-error cleanup-error recovery-world error)))))
  nil)

(defn- sweep-managed! [^DenseWorld world]
  (let [cells (current-cells world)]
    (doseq [{:keys [owner-ref slots]}
            (:managed-descriptors @(.-registry world))
            :when (not (live-managed-owner? owner-ref))
            slot slots
            :when (< slot (cells-length cells))]
      (cells-set! cells slot absent))))

(defn managed-value
  "Read a self-described managed slot in the active related world, falling
  back to the owner's creation world outside managed evaluation."
  [home registry slot]
  (let [state (execution/current-state)]
    (if (identical? registry (execution/active-registry state))
      (do
        (assert-open! (selected-world))
        (let [cells @(execution/active-cells-holder state)
              value (if (>= slot (cells-length cells))
                      absent
                      (cells-get cells slot))]
          (if (identical? value absent)
            (slot-value home slot absent)
            value)))
      (slot-value home slot absent))))

(defn managed-value-in
  "Read a managed slot from an already selected world."
  [world home slot]
  (if (identical? world home)
    (slot-value world slot absent)
    (let [value (slot-value world slot absent)]
      (if (identical? value absent)
        (slot-value home slot absent)
        value))))

(defn- call-with-managed-mutation [^DenseWorld home registry f]
  (let [active (active-world-state)
        related? (and active
                      (identical? registry
                                  (.-registry ^DenseWorld
                                              (.-world ^ReadWorld active))))
        ^DenseWorld world (if related? (.-world ^ReadWorld active) home)]
    (with-world-access world #(f world))))

(defn managed-swap!
  "CAS a directly described slot. The logical fallback represents a slot
  allocated in another branch but not yet realized in the selected world."
  ([home registry slot f args validate! notify!]
   (managed-swap! home registry slot f args validate! notify! nil nil))
  ([home registry slot f args validate! notify! coordination]
   (managed-swap! home registry slot f args validate! notify!
                  coordination nil))
  ([home registry slot f args validate! notify! coordination validation-current?]
   (call-with-managed-mutation
    home registry
    (fn [^DenseWorld world]
      (ensure-capacity! world (inc slot))
      (loop []
        (let [cells (current-cells world)
              raw-old (cells-get cells slot)
              old (if (identical? absent raw-old)
                    (slot-value home slot absent)
                    raw-old)
              new (if (nil? args) (f old) (apply f old args))
              validation (validate! world new)
              committed
              (with-coordination
                coordination
                #(with-cells-read-lock
                   world
                   (fn []
                     (let [cells (current-cells world)
                           current (cells-get cells slot)]
                       (if (or (not (identical? current raw-old))
                               (and validation-current?
                                    (not (validation-current? world validation))))
                         ::retry
                         (if (cells-cas! cells slot current new)
                           [validation old new]
                           ::retry))))))]
          (if (identical? ::retry committed)
            (recur)
            (let [[validation old new] committed]
              (notify! world validation old new)
              new))))))))

(defn managed-reset!
  ([home registry slot new validate! notify!]
   (managed-reset! home registry slot new validate! notify! nil nil))
  ([home registry slot new validate! notify! coordination]
   (managed-reset! home registry slot new validate! notify! coordination nil))
  ([home registry slot new validate! notify! coordination validation-current?]
   (managed-swap! home registry slot (constantly new) nil
                  validate! notify! coordination validation-current?)))

(defn managed-assoc!
  "Associate one key in a managed persistent-map slot without the generic
  callback/validation path. Used by mutable SCI deftype fields."
  [home registry slot k value]
  (call-with-managed-mutation
   home registry
   (fn [^DenseWorld world]
     (ensure-capacity! world (inc slot))
     (with-cells-read-lock
       world
       #(loop []
          (let [cells (current-cells world)
                raw-old (cells-get cells slot)
                old (if (identical? absent raw-old)
                      (slot-value home slot {})
                      raw-old)
                new (assoc old k value)]
            (if (cells-cas! cells slot raw-old new)
              value
              (recur))))))))

(defn managed-compare-and-set!
  ([home registry slot expected new validate! notify!]
   (managed-compare-and-set! home registry slot expected new
                             validate! notify! nil nil))
  ([home registry slot expected new validate! notify! coordination]
   (managed-compare-and-set! home registry slot expected new
                             validate! notify! coordination nil))
  ([home registry slot expected new validate! notify! coordination validation-current?]
   (call-with-managed-mutation
    home registry
    (fn [^DenseWorld world]
      (ensure-capacity! world (inc slot))
      (let [[raw-old old]
            (with-cells-read-lock
              world
              #(let [raw-old (cells-get (current-cells world) slot)]
                 [raw-old (if (identical? absent raw-old)
                            (slot-value home slot absent)
                            raw-old)]))]
        (if-not (identical? expected old)
          false
          (let [validation (validate! world new)
                result
                (with-coordination
                  coordination
                  #(with-cells-read-lock
                     world
                     (fn []
                       (let [cells (current-cells world)]
                         (if (or (not (identical? raw-old
                                                 (cells-get cells slot)))
                                 (and validation-current?
                                      (not (validation-current? world validation))))
                           false
                           (if (cells-cas! cells slot raw-old new)
                             [validation old new]
                             false))))))]
            (if (vector? result)
              (let [[validation old new] result]
                (notify! world validation old new)
                true)
              false))))))))

(defn managed-update-control-with-value!
  "Atomically validate the selected value and update a related control slot.
  `coordination` must be the same stable handle used by value commits."
  [home registry value-slot control-slot coordination validate-value! update-control]
  (call-with-managed-mutation
   home registry
   (fn [^DenseWorld world]
     (ensure-capacity! world (inc (max value-slot control-slot)))
     (loop []
       (let [{:keys [raw-value raw-control value control]}
             (with-cells-read-lock
               world
               #(let [cells (current-cells world)
                      raw-value (cells-get cells value-slot)
                      raw-control (cells-get cells control-slot)]
                  {:raw-value raw-value
                   :raw-control raw-control
                   :value (if (identical? absent raw-value)
                            (slot-value home value-slot absent)
                            raw-value)
                   :control (if (identical? absent raw-control)
                              (slot-value home control-slot absent)
                              raw-control)}))
             _ (validate-value! value)
             result
             (with-coordination
               coordination
               #(with-cells-read-lock
                  world
                  (fn []
                    (let [cells (current-cells world)]
                      (if (or (not (identical? raw-value
                                              (cells-get cells value-slot)))
                              (not (identical? raw-control
                                              (cells-get cells control-slot))))
                        ::retry
                        (if (cells-cas! cells control-slot raw-control
                                        (update-control control))
                          ::committed
                          ::retry))))))]
         (if (identical? ::retry result)
           (recur)
           nil))))))

(defn register!
  ([handle value]
   (register! handle value nil))
  ([handle value value-fork-fn]
   (when-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (allocate-descriptor! world handle :ref true value-fork-fn)]
       (ensure-capacity! world (inc value-slot))
       (with-cells-read-lock
         world #(cells-set! (current-cells world) value-slot value))))
   handle))

(defn- allocate-var-watch-slot! [^DenseWorld world handle fallback]
  (let [registry (.-registry world)]
    (with-registry-lock
      registry
      (fn []
        (let [desc (descriptor world handle)]
          (if (:watch-slot desc)
            desc
            (let [slot (:next-slot @registry)
                  desc (assoc desc
                              :watch-slot slot
                              :watch-fallback fallback)]
              (swap! registry
                     (fn [r]
                       (-> r
                           (assoc :next-slot (inc slot))
                           (assoc-in [:descriptors handle] desc))))
              desc)))))))

(defn register-var!
  ([handle value m]
   (register-var! handle value m nil))
  ([handle value m watches]
   (when-let [^DenseWorld world (current-world)]
     (-mark-world-tracked! handle
                           (when-not (:sci/built-in m) store/*ctx*))
     (let [{:keys [value-slot meta-slot]}
           (allocate-descriptor! world handle :var (not (:sci/built-in m))
                                 (:sci.impl/fork-fn m))]
       (ensure-capacity! world (inc meta-slot))
       (with-cells-read-lock
         world
         #(let [cells (current-cells world)]
            (cells-set! cells value-slot value)
            (cells-set! cells meta-slot m)))
       (when (seq watches)
           (let [{:keys [watch-slot]}
               (allocate-var-watch-slot! world handle watches)]
           (ensure-capacity! world (inc watch-slot))
           (with-cells-read-lock
             world #(cells-set! (current-cells world) watch-slot watches))))))
   handle))

(defn register-namespace! [handle m]
  (when-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (allocate-descriptor! world handle :namespace false nil)]
      (ensure-capacity! world (inc value-slot))
      (with-cells-read-lock
        world
        #(let [cells (current-cells world)]
           (when (identical? absent (cells-get cells value-slot))
             (cells-set! cells value-slot m))))))
  handle)

(defn register-type!
  ([handle data]
   (register-type! handle data nil))
  ([handle data value-fork-fn]
   (when-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (allocate-descriptor! world handle :type false value-fork-fn)]
       (ensure-capacity! world (inc value-slot))
       (with-cells-read-lock
         world
         #(let [cells (current-cells world)]
            (when (identical? absent (cells-get cells value-slot))
              (cells-set! cells value-slot data))))))
   handle))

(defn- reset-slot! [^DenseWorld world slot value]
  (if (nil? slot)
    ::no-world
    (do
      (ensure-capacity! world (inc slot))
      (with-cells-read-lock
        world #(cells-set! (current-cells world) slot value)))))

(defn reset-value! [handle value]
  (if-let [world (current-world)]
    (reset-slot! world (:value-slot (descriptor world handle)) value)
    ::no-world))

(defn reset-var-meta! [handle m]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [meta-slot]} (or (descriptor world handle)
                                  (allocate-descriptor! world handle :var
                                                        (not (:sci/built-in m))
                                                        (:sci.impl/fork-fn m)))]
      (reset-slot! world meta-slot m))
    ::no-world))

(defn reset-namespace-meta! [handle m]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :namespace false nil))]
      (reset-slot! world value-slot m))
    ::no-world))

(defn reset-type-data! [handle data]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :type false nil))]
      (reset-slot! world value-slot data))
    ::no-world))

(defn alter-var-meta! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [meta-slot]} (or (descriptor world handle)
                                  (allocate-descriptor! world handle :var
                                                        (not (:sci/built-in fallback))
                                                        (:sci.impl/fork-fn fallback)))]
      (ensure-capacity! world (inc meta-slot))
      (with-cells-read-lock
        world
        #(loop []
           (let [cells (current-cells world)
                 raw-old (cells-get cells meta-slot)
                 old (if (identical? absent raw-old) fallback raw-old)
                 new (apply f old args)]
             (if (cells-cas! cells meta-slot raw-old new) new (recur))))))
    ::no-world))

(defn alter-namespace-meta! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :namespace false nil))]
      (ensure-capacity! world (inc value-slot))
      (with-cells-read-lock
        world
        #(loop []
           (let [cells (current-cells world)
                 raw-old (cells-get cells value-slot)
                 old (if (identical? absent raw-old) fallback raw-old)
                 new (apply f old args)]
             (if (cells-cas! cells value-slot raw-old new) new (recur))))))
    ::no-world))

(defn alter-type-data! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [value-slot]}
          (or (descriptor world handle)
              (allocate-descriptor! world handle :type false nil))]
      (ensure-capacity! world (inc value-slot))
      (with-cells-read-lock
        world
        #(loop []
           (let [cells (current-cells world)
                 raw-old (cells-get cells value-slot)
                 old (if (identical? absent raw-old) fallback raw-old)
                 new (apply f old args)]
             (if (cells-cas! cells value-slot raw-old new) new (recur))))))
    ::no-world))

(defn alter-var-watches! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [watch-slot]}
          (allocate-var-watch-slot! world handle fallback)]
      (ensure-capacity! world (inc watch-slot))
      (with-cells-read-lock
        world
        #(loop []
           (let [cells (current-cells world)
                 raw-old (cells-get cells watch-slot)
                 old (if (identical? absent raw-old) fallback raw-old)
                 new (apply f old args)]
             (if (cells-cas! cells watch-slot raw-old new) new (recur))))))
    ::no-world))

(defn swap-value!
  "Atomically update one tracked slot. Unrelated SCI atoms do not contend and
  cannot cause `f` to be retried."
  [handle f args validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))]
    (loop []
      (let [cells (current-cells world)
            old (cells-get cells slot)
            new (apply f old args)
            committed (with-cells-read-lock
                        world
                        #(let [cells (current-cells world)]
                           (if-not (identical? old (cells-get cells slot))
                             false
                             (do
                               (validate! new)
                               (cells-cas! cells slot old new)))))]
        (if committed
          (do (notify! old new) new)
          (recur))))))

(defn reset-tracked! [handle new validate! notify!]
  (swap-value! handle (constantly new) nil validate! notify!))

(defn compare-and-set-tracked! [handle expected new validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))
        result (with-cells-read-lock
                 world
                 #(let [cells (current-cells world)
                        old (cells-get cells slot)]
                    (if-not (identical? expected old)
                      ::failed
                      (do
                        (validate! new)
                        (if (cells-cas! cells slot old new) old ::failed)))))]
    (if (identical? ::failed result)
      false
      (do (notify! result new) true))))

(defn active-ctx
  "Use the currently evaluating descendant context for an interpreted closure.
  Unrelated SCI contexts cannot capture one another accidentally."
  [captured]
  (let [active store/*ctx*]
    (if (and active
             (identical? (:sci.impl/lineage captured)
                         (:sci.impl/lineage active)))
      active
      captured)))

(defn- realize-primary! [^DenseWorld world snapshot-value snapshot-var-meta]
  (when-not @(.-persistent? world)
    (doseq [[handle {:keys [kind value-slot meta-slot]}]
            (:descriptors @(.-registry world))]
      (when (present-slot? world value-slot)
        (cells-set! (current-cells world) value-slot (snapshot-value handle))
        (when (= :var kind)
          (cells-set! (current-cells world) meta-slot (snapshot-var-meta handle)))))
    (vreset! (.-persistent? world) true))
  world)

(defn- call-with-selected-world [^DenseWorld world f]
  (let [state #?(:cljd (execution/current-state)
                 :clj (.get ^ThreadLocal execution/current)
                 :cljs (execution/current-state))
        previous (execution/active-world state)
        previous-scope (execution/binding-scope state)]
    (install-active-world! state (ReadWorld. world false))
    (execution/set-binding-scope! state world)
    (execution/refresh-active-bindings! state)
    (try
      (f)
      (finally
        (install-active-world! state previous)
        (execution/set-binding-scope! state previous-scope)
        (execution/refresh-active-bindings! state)))))

(defn- copy-world [^DenseWorld world fork-value]
  (sweep-managed! world)
  (let [source (current-cells world)
        registry (.-registry world)
        registry-state @registry
        logical-n (:next-slot registry-state)
        target (new-cells (max 16 logical-n))
        descriptors (:descriptors registry-state)
        managed-descriptors (:managed-descriptors registry-state)
        ;; Membership concerns stable handle identity. Hashing arbitrary root
        ;; values here can realize managed lazy sequences before fork-value
        ;; even sees them (and can run arbitrary host hash/equals methods).
        handles #?(:clj (let [index (IdentityHashMap.)]
                          (doseq [handle (keys descriptors)]
                            (.put index handle true))
                          index)
                    :cljs (js/Set. (to-array (keys descriptors)))
                    :cljd (vec (keys descriptors)))
        handle? (fn [value]
                  #?(:clj (.containsKey ^IdentityHashMap handles value)
                     :cljs (.has handles value)
                     :cljd (boolean (some #(identical? % value) handles))))
        target-world (DenseWorld.
                      (volatile! target)
                      registry
                      #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
                      #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
                      (volatile! true)
                      (volatile! true)
                      (volatile! 0))]
    (dotimes [i logical-n]
      ;; The registry is shared by siblings, but each world's physical array
      ;; only grows when that world allocates. Sibling-only slots are absent.
      (cells-set! target i (if (< i (cells-length source))
                            (cells-get source i)
                            absent)))
    (try
      (when fork-value
        ;; Establish target-local type/control realizations before general
        ;; Forkable values inspect the selected child world.
        (doseq [[_ {:keys [value-slot value-fork-fn]}] descriptors
                :when value-fork-fn
                :when (< value-slot logical-n)
                :let [v (cells-get target value-slot)]
                :when (not (identical? absent v))]
          (cells-set! target value-slot (value-fork-fn v))))
      (call-with-selected-world
       target-world
       (fn []
         (when fork-value
           (doseq [[_ {:keys [value-slot host-fork? value-fork-fn]}]
                   descriptors
                   :when (and host-fork? (nil? value-fork-fn))
                   :when (< value-slot logical-n)
                   :let [v (cells-get target value-slot)]
                   :when (not (identical? absent v))]
             (cells-set! target value-slot
                         (if (handle? v) v (fork-value v))))
           (doseq [{:keys [fork-slots]} managed-descriptors
                   slot fork-slots
                   :when (< slot logical-n)
                   :let [v (cells-get target slot)]
                   :when (not (identical? absent v))]
             (cells-set! target slot (fork-value v))))
         ;; Anonymous cache owners participate without reserving a dense slot
         ;; per lazy node. Their snapshots never invoke deferred computations.
         (doseq [owner (world-cache-owners world)]
           (-fork-world-cache! owner world target-world))))
      target-world
      (catch #?(:clj Throwable :cljs :default :cljd Object) error
        (try
          (retire-world! target-world)
          (catch #?(:clj Throwable :cljs :default :cljd Object) cleanup-error
            (throw (fork-cleanup-failure error cleanup-error target-world nil))))
        (throw error)))))

(defn fork-world [^DenseWorld world fork-value snapshot-value snapshot-var-meta]
  #?(:cljd
     (do
       (assert-quiescent! world :fork)
       (with-counted-world-access
         world
         #(do (realize-primary! world snapshot-value snapshot-var-meta)
              (copy-world world fork-value))))
     :clj
     (let [^ReentrantReadWriteLock gate (.-gate world)]
       (when (or (pos? (.getReadHoldCount gate))
                 (.isWriteLockedByCurrentThread gate))
         (throw (IllegalStateException.
                 "Cannot fork a SCI world from inside its active evaluation or copy; suspend it first.")))
       (let [lock (.writeLock gate)]
         (.lock lock)
         (try
           (assert-open! world)
           (realize-primary! world snapshot-value snapshot-var-meta)
           (copy-world world fork-value)
           (finally (.unlock lock)))))
     :cljs
     (do
       (assert-quiescent! world :fork)
       (with-counted-world-access
         world
         #(do (realize-primary! world snapshot-value snapshot-var-meta)
              (copy-world world fork-value))))))
