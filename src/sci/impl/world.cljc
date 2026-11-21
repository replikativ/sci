(ns sci.impl.world
  "Dense, fork-local runtime state for SCI contexts.

  Stable handles are assigned integer slots once per context lineage. A world
  stores slot values densely; a fork copies that array. Mutable primitives CAS
  only their own slot, so unrelated atoms never contend on a shared world root.

  Locking (JVM). Evaluation takes no world lock and reads slots lock-free.
  - `slot-gate` (read): every slot write, together with the host-object
    mirror of a primary world (Var root/meta, Namespace meta, Type data,
    volatile) and a lazy realization step. No user code runs inside.
  - `publish-gate` (read): a publication of several slot writes that must
    appear together, e.g. a def's value and metadata. No user code runs
    inside: Var watch notifications are deferred until it ends, and the
    namespace-environment atom (whose watches and validator are user code)
    is never updated inside.
  - A fork takes both write locks only for the copy: primary realization, the
    cells, the lazy realization caches and the namespace environment. It
    excludes writers for that copy and never waits for an evaluation.
  - A resize takes `slot-gate` exclusively for the array copy.
  A fork is therefore a cut between individual publications. Every
  publication makes its result reachable last: slots are written before an
  environment mapping or slot value refers to the handle."
  (:require [sci.ctx-store :as store]
            [sci.fork :as fork]
            [sci.impl.execution :as execution])
  #?@(:cljd []
      :clj [(:import [java.lang.ref WeakReference]
                     [java.util IdentityHashMap WeakHashMap]
                     [java.util.concurrent.atomic AtomicReferenceArray]
                     [java.util.concurrent.locks ReentrantReadWriteLock])]))

(def absent #?(:cljd (Object.) :clj (Object.) :cljs (js/Object.)))

;; `gate` is retained for embedders that read the field; SCI no longer locks
;; it, so a read permit taken there never delays a fork or a retirement.
;; `retirement` is nil while open, else an atom of
;; {:retained {owner realization} :done? bool}.
;; `in-flight` counts evaluations and host accesses inside the world; a
;; retired world is cleared (cells, retained caches) when it drops to zero.
(deftype DenseWorld [cells-holder registry gate slot-gate publish-gate
                     persistent? open? retirement in-flight])

(defprotocol IForkableWorldCache
  (-fork-world-cache! [owner source target]
    "Copy an existing source realization into the unpublished target without
     invoking deferred computation. Missing source state remains missing.
     Called inside the fork's copy section: it must not block or run user
     code."))

(defprotocol IWorldCache
  (-retire-world-cache! [owner world]
    "Release this managed owner's cache for one retired world.
     This internal hook only removes private cache entries: no user callbacks
     or I/O. It must be idempotent so failed retirement can be retried."))

(defprotocol IRetainedWorldCache
  (-world-cache-entry [owner world]
    "This owner's realization for `world`, or nil. Retirement keeps it in the
     retired world itself, so evaluations still running there find it after
     the owner released it."))

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

(defn- with-cells-read-lock
  "A slot write section: excluded by a resize or a fork's copy. `f` must not
   run user code or wait on anything but these locks."
  [^DenseWorld world f]
  #?(:cljd (f)
     :clj (let [lock (.readLock ^ReentrantReadWriteLock (.-slot-gate world))]
            (.lock lock)
            (try (f) (finally (.unlock lock))))
     :cljs (f)))

(declare selected-world)

(defn with-selected-slot-section
  "Run `f` (no user code) as one slot write of the selected world, so a fork's
   copy sees it entirely or not at all. Used for lazy realization steps."
  [f]
  (if-let [world (selected-world)]
    (with-cells-read-lock world f)
    (f)))

(defn- with-cells-write-lock [^DenseWorld world f]
  #?(:cljd (f)
     :clj (let [lock (.writeLock ^ReentrantReadWriteLock (.-slot-gate world))]
            (.lock lock)
            (try (f) (finally (.unlock lock))))
     :cljs (f)))

;; Callbacks (Var watches) reached inside a publication run after it ends.
;; JVM: per thread [depth deferred-thunks]; elsewhere one state.
#?(:clj
   (def ^:private ^ThreadLocal publication-state
     (ThreadLocal/withInitial
      (reify java.util.function.Supplier
        (get [_] (object-array [0 nil])))))
   :default
   (def ^:private publication-state (volatile! (object-array [0 nil]))))

(defn- current-publication-state []
  #?(:clj (.get ^ThreadLocal publication-state)
     :default @publication-state))

(defn run-after-publication!
  "Run `thunk` now, or after the publication this thread is inside ends.
   Used for user callbacks (watches) triggered by a published write."
  [thunk]
  (let [^objects state (current-publication-state)]
    (if (pos? (long (aget state 0)))
      (aset state 1 (conj (or (aget state 1) []) thunk))
      (thunk))
    nil))

(defn- run-deferred! [^objects state]
  (let [thunks (aget state 1)]
    (aset state 1 nil)
    (reduce (fn [error thunk]
              (try (thunk) error
                   (catch #?(:clj Throwable :cljs :default :cljd Object) e
                     (or error e))))
            nil thunks)))

(defn publishing-in
  "Run `f` as one publication in `world`: a fork's copy observes all of its
   writes or none. `f` must not run user code; callbacks it triggers through
   `run-after-publication!` run after the publication, in order."
  [^DenseWorld world f]
  (if (nil? world)
    (f)
    (let [^objects state (current-publication-state)
          depth (aget state 0)
          run #(do (aset state 0 #?(:clj (num (inc (long depth)))
                                    :default (inc depth)))
                   (try (f) (finally (aset state 0 depth))))
          [ok result]
          (try
            [true #?(:clj (let [lock (.readLock ^ReentrantReadWriteLock (.-publish-gate world))]
                            (.lock lock)
                            (try (run) (finally (.unlock lock))))
                     :default (run))]
            (catch #?(:clj Throwable :cljs :default :cljd Object) e [false e]))
          deferred-error (when (zero? (long depth)) (run-deferred! state))]
      (cond (not ok) (throw result)
            deferred-error (throw deferred-error)
            :else result))))

;; Writers of one handle's slot pair (a def's value and metadata) are
;; serialized by a striped monitor, so two definitions never interleave into
;; one's value with the other's metadata. `f` runs no user code and takes no
;; other monitor that may be held across user code.
#?(:clj
   (def ^:private handle-locks
     (let [a (object-array 64)]
       (dotimes [i 64] (aset a i (Object.)))
       a)))

(defn with-handle-lock [handle f]
  #?(:clj (locking (aget ^objects handle-locks (bit-and (System/identityHashCode handle) 63))
            (f))
     :default (f)))

(declare current-world)

(defn publishing
  "`publishing-in` the world of SCI's current context, or plain `(f)` outside
   a forkable context."
  [f]
  (publishing-in (current-world) f))

(defn- with-copy-section
  "The fork's copy: excludes every publication and slot write of `world`,
   never an evaluation."
  [^DenseWorld world f]
  #?(:clj
     (let [^ReentrantReadWriteLock publish (.-publish-gate world)
           ^ReentrantReadWriteLock slots (.-slot-gate world)]
       (when (or (pos? (.getReadHoldCount publish))
                 (pos? (.getReadHoldCount slots)))
         (throw (IllegalStateException.
                 "Cannot fork a SCI world from inside one of its publications.")))
       (let [p (.writeLock publish)
             c (.writeLock slots)]
         (.lock p)
         (try
           (.lock c)
           (try (f) (finally (.unlock c)))
           (finally (.unlock p)))))
     :default (f)))

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

(defn- dense-world [cells registry persistent?]
  (DenseWorld.
   (volatile! cells)
   registry
   #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
   #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
   #?(:cljd nil :clj (ReentrantReadWriteLock.) :cljs nil)
   (volatile! persistent?)
   (volatile! true)
   (atom nil)
   #?(:clj (java.util.concurrent.atomic.AtomicLong.) :default (volatile! 0))))

(defn new-world []
  (dense-world (new-cells 64)
               (atom {:next-slot 0 :descriptors {} :managed-descriptors []
                      :world-caches (new-world-cache-index)})
               false))

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
  (when-let [^ReadWorld active (active-world-state)]
    (.-world active)))

(declare assert-accessible! record-late-cache-owner!)

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
    (assert-accessible! world)
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
         :cljd nil))
      ;; After registering: a retirement that closed the world earlier may
      ;; have scanned the owners already; its final clear releases this one.
      (when-not @(.-open? world)
        (record-late-cache-owner! world owner)))
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

;; Worlds an evaluation on this thread was admitted to (`call-admitted`):
;; e.g. the forms of one eval-string*, each entering the world separately.
#?(:clj
   (def ^:private ^ThreadLocal admitted (ThreadLocal.))
   :default
   (def ^:private admitted (volatile! nil)))

(defn- admitted-worlds []
  #?(:clj (.get ^ThreadLocal admitted) :default @admitted))

(defn- assert-accessible!
  "Entering or reading `world` from outside it requires an open world. An
   evaluation already running in it (selected, or admitted on this thread)
   finishes there even after retirement."
  [^DenseWorld world]
  (when-not @(.-open? world)
    (when-not (or (identical? world (selected-world))
                  (some #(identical? world %) (admitted-worlds)))
      (assert-open! world)))
  world)

(declare reclaim-if-idle!)

(defn- enter-world! [^DenseWorld world]
  #?(:clj (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong (.-in-flight world))
     :default (vswap! (.-in-flight world) inc)))

(defn- leave-world! [^DenseWorld world]
  (when (zero? #?(:clj (.decrementAndGet ^java.util.concurrent.atomic.AtomicLong (.-in-flight world))
                  :default (vswap! (.-in-flight world) dec)))
    (when-not @(.-open? world)
      ;; A failing cache owner keeps its cleanup pending; retire! retries it
      ;; (and reports the failure). It must not replace this evaluation's
      ;; own outcome.
      (try (reclaim-if-idle! world)
           (catch #?(:clj Throwable :cljs :default :cljd Object) _ nil)))))

(defn- in-flight [^DenseWorld world]
  #?(:clj (.get ^java.util.concurrent.atomic.AtomicLong (.-in-flight world))
     :default @(.-in-flight world)))

(defn- with-counted-entry
  "Run `f` as an entry into `world` from outside it: counted, so a retired
   world is cleared only after its last evaluation left. The count is taken
   before the open check: a retirement either sees it or this entry sees the
   world closed (and is refused, unless already admitted)."
  [^DenseWorld world f]
  (enter-world! world)
  (try
    (assert-accessible! world)
    (f)
    (finally (leave-world! world))))

(defn call-admitted
  "Run `f`, an evaluation of `ctx` that may enter its world several times
   (one eval-string*), as one evaluation: admitted while the world is open,
   it finishes even if the world is retired meanwhile."
  [ctx f]
  (if-let [world (:sci.impl/world ctx)]
    (with-counted-entry
      world
      (fn []
        (let [previous (admitted-worlds)]
          #?(:clj (.set ^ThreadLocal admitted (cons world previous))
             :default (vreset! admitted (cons world previous)))
          (try (f)
               (finally
                 #?(:clj (.set ^ThreadLocal admitted previous)
                    :default (vreset! admitted previous)))))))
    (f)))

(defn- with-world-access [^DenseWorld world f]
  ;; Inside the world (selected) the enclosing entry is already counted.
  (if (identical? world (selected-world))
    (f)
    (with-counted-entry world f)))

;; Worlds whose fork callbacks run on this thread. Such a callback can neither
;; fork nor retire its source: that would recurse into the same copy.
#?(:clj
   (def ^:private ^ThreadLocal copying (ThreadLocal.))
   :default
   (def ^:private copying (volatile! nil)))

(defn- copying-worlds []
  #?(:clj (.get ^ThreadLocal copying) :default @copying))

(defn- set-copying-worlds! [worlds]
  #?(:clj (.set ^ThreadLocal copying worlds) :default (vreset! copying worlds)))

(defn- with-copying [^DenseWorld world f]
  (let [previous (copying-worlds)]
    (set-copying-worlds! (cons world previous))
    (try (f) (finally (set-copying-worlds! previous)))))

(defn- assert-not-copying! [^DenseWorld world operation]
  (when (some #(identical? world %) (copying-worlds))
    (throw (ex-info (str "Cannot " (name operation)
                         " a SCI world from inside its active evaluation or copy.")
                    {:type ::active-world :operation operation}))))

(defn with-active-world
  "Run `f` with the context's world installed. Evaluation takes no world
  lock: a fork excludes only slot writers, for its copy, and a retirement
  only refuses new entries."
  [ctx f]
  (if-let [world (:sci.impl/world ctx)]
    (with-counted-entry world #(call-with-active-world ctx f))
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
  ;; one snapshot: a retirement may replace the array between two reads
  (let [cells (current-cells world)]
    (and (some? slot)
         (< slot (cells-length cells))
         (not (identical? absent (cells-get cells slot))))))

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
  "Read a Var through a slot already resolved by the analyzer: its value in
   the active world, `absent` when that world has no realization of it (the
   caller then reads the Var fully, which consults its home), or `fallback`
   outside a forked world."
  [slot fallback]
  (if-let [holder (execution/active-cells-holder)]
    (let [cells @holder]
      (if (>= slot (cells-length cells))
        absent
        (cells-get cells slot)))
    fallback))

(declare read-through-home)

(defn- var-slot-read
  "Read a Var's value or meta slot in its selected world. When that world
   (a related one: parent, sibling) has no realization of the Var, its home
   world answers: its value while open, the retired error once retired,
   never the Var's raw root. The presence check runs inside the counted
   access, on one cells snapshot."
  [handle fallback slot-key]
  (let [ctx (selected-var-ctx handle)
        ^ReadWorld active (:sci.impl/read-world ctx)]
    (if (nil? active)
      fallback
      (let [^DenseWorld world (.-world active)
            slot (slot-key (descriptor world handle))]
        (with-world-access
         world
         (fn []
           (let [cells (current-cells world)
                 v (if (and (some? slot) (< slot (cells-length cells)))
                     (cells-get cells slot)
                     absent)]
             (cond
               (not (identical? absent v)) (if (.-primary? active) fallback v)
               :else (read-through-home handle world slot fallback)))))))))

(defn- read-through-home
  "The selected `world` has no realization of `handle`'s `slot`: read it in
   the handle's home world if that is another one (refused once retired),
   else `fallback`."
  [handle ^DenseWorld world slot fallback]
  (let [^ReadWorld home-read (:sci.impl/read-world (-world-home-ctx handle))]
    (if (and home-read (not (identical? world (.-world home-read))))
      (let [^DenseWorld home-world (.-world home-read)]
        (with-world-access
         home-world
         #(if (.-primary? home-read)
            fallback
            (slot-value home-world slot fallback))))
      fallback)))

(defn var-value
  "Primary contexts retain their direct Var realization. Descendants resolve
  the stable Var handle through its dense lineage slot."
  [handle fallback]
  (var-slot-read handle fallback :value-slot))

(defn var-meta
  "A Var's metadata in its selected world. Unlike its value, metadata without
   a realization there falls back to the Var's own field (name, namespace),
   so printing a retired world's Var still works."
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
  (assert-accessible! world)
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
    (assert-accessible! world)
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

(defn retired-cache-entry
  "The realization `owner` held for the retired `world` when it was retired,
   or nil. Evaluations still running in a retired world read their caches
   here after the owners released them."
  [^DenseWorld world owner]
  (when-let [r @(.-retirement world)]
    (get (:retained @r) owner)))

(defn- retirement-state [^DenseWorld world]
  (or @(.-retirement world)
      (do (compare-and-set! (.-retirement world) nil
                            (atom {:retained {} :late [] :done? false}))
          @(.-retirement world))))

(defn- record-late-cache-owner!
  "A cache owner registered into a world after it was closed (by an
   evaluation still running there): released when the world is cleared."
  [^DenseWorld world owner]
  (swap! (retirement-state world) update :late (fnil conj []) owner))

(defn retire-world!
  "Retire one interpreter world now. New evaluations and forks are refused;
   evaluations already running in it finish against it. Managed cache owners
   release their world-specific state (the world keeps what running
   evaluations still need). When no evaluation is in the world, now or once
   the last one leaves, its cells and retained caches are cleared; the small
   closed world remains for Vars whose home it is, which then refuse access.
   This does not close foreign host resources or retire sibling worlds. Safe
   to repeat after a failed cache cleanup."
  [^DenseWorld world]
  (assert-not-copying! world :retire)
  (vreset! (.-open? world) false)
  (let [r (retirement-state world)]
    (when-not (:done? @r)
      ;; A slot section: a fork whose copy is in progress copies the caches
      ;; before they are released (this waits for that copy only); a fork
      ;; starting later finds the world closed inside its own copy section.
      (with-cells-read-lock
        world
        (fn []
          (let [owners (retirement-cache-owners world)
                retained (reduce (fn [m owner]
                                   (if-let [entry (when (satisfies? IRetainedWorldCache owner)
                                                    (-world-cache-entry owner world))]
                                     (assoc m owner entry)
                                     m))
                                 {} owners)]
            ;; Published before any owner releases its entry: a running
            ;; evaluation that misses the owner's entry finds this one.
            (swap! r update :retained merge retained)
            (doseq [owner owners]
              (-retire-world-cache! owner world))
            (swap! r assoc :done? true)))))
    ;; Also on a retry after the scan completed: a final clear may still be
    ;; pending (a late cache owner whose cleanup failed).
    (when (zero? (in-flight world))
      (reclaim-if-idle! world)))
  nil)

(defn- reclaim-if-idle!
  "Clear a retired world once no evaluation is in it: its cells and the
   caches retirement kept for running evaluations. The closed world object
   stays as long as a Var's home refers to it, so a later access through
   such a Var is refused, never answered from a fallback. Runs in a slot
   section (after a fork copy in progress); idempotent."
  [^DenseWorld world]
  (when-let [r @(.-retirement world)]
    (when (:done? @r)
      (with-cells-read-lock
        world
        (fn []
          (when (zero? (in-flight world))
            (vreset! (.-cells-holder world) (new-cells 0))
            (swap! r assoc :retained {})
            ;; One at a time: a failure leaves only the owners still pending,
            ;; for retire! to retry.
            (doseq [owner (:late @r)]
              (-retire-world-cache! owner world)
              (swap! r update :late (fn [late] (into [] (remove #(identical? owner %)) late)))))))))
  nil)

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
      ;; The active world: an evaluation running there finishes even after
      ;; the world was retired.
      (do
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

(defn- mirror!
  "Inside a slot section, after `slot` was written: make the host object of a
   primary world (`mirror` writes it, no user code) agree with the slot.
   Concurrent writers each re-read the slot after mirroring it, so the last
   mirror leaves the host object equal to the slot."
  [^DenseWorld world slot mirror]
  (when mirror
    (loop []
      (let [v (cells-get (current-cells world) slot)]
        (mirror v)
        (when-not (identical? v (cells-get (current-cells world) slot))
          (recur))))))

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
  "Write a Var's value and metadata (and watches) in one slot section.
   `mirror-root`, when given, writes the primary world's Var root in that
   same section (see `mirror!`)."
  ([handle value m]
   (register-var! handle value m nil nil))
  ([handle value m watches]
   (register-var! handle value m watches nil))
  ([handle value m watches mirror-root]
   (when-let [^DenseWorld world (current-world)]
     (-mark-world-tracked! handle
                           (when-not (:sci/built-in m) store/*ctx*))
     (let [{:keys [value-slot meta-slot]}
           (allocate-descriptor! world handle :var (not (:sci/built-in m))
                                 (:sci.impl/fork-fn m))
           watch-slot (when (seq watches)
                        (:watch-slot (allocate-var-watch-slot! world handle watches)))]
       (ensure-capacity! world (inc (max meta-slot (or watch-slot 0))))
       (with-cells-read-lock
         world
         #(let [cells (current-cells world)]
            (cells-set! cells value-slot value)
            (cells-set! cells meta-slot m)
            (when watch-slot (cells-set! cells watch-slot watches))
            (mirror! world value-slot mirror-root)))))
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

(defn- reset-slot! [^DenseWorld world slot value mirror]
  (if (nil? slot)
    ::no-world
    (do
      (ensure-capacity! world (inc slot))
      (with-cells-read-lock
        world #(do (cells-set! (current-cells world) slot value)
                   (mirror! world slot mirror)
                   value)))))

(defn reset-value!
  "Reset a tracked slot; `mirror` (optional) writes the primary world's host
   object in the same slot section."
  ([handle value] (reset-value! handle value nil))
  ([handle value mirror]
   (if-let [world (current-world)]
     (reset-slot! world (:value-slot (descriptor world handle)) value mirror)
     ::no-world)))

(defn- primary-slot [^DenseWorld world handle]
  (when-let [slot (:value-slot (descriptor world handle))]
    (when (present-slot? world slot) slot)))

(defn mutable-primary-write!
  "Write `value` to the host object of `handle` (a volatile) in a
   still-mutable primary world: `(write-root! value)` inside one slot
   section, so the first fork's realization (which holds the section
   exclusively) reads the root before or after the write, never in between
   related writes. If the realization happened meanwhile, the write takes the
   realized path: slot first, then the root mirrored from it."
  [handle value write-root!]
  (if-let [^ReadWorld active (active-world-state)]
    (let [^DenseWorld world (.-world active)]
      (with-cells-read-lock
        world
        #(if-let [slot (when @(.-persistent? world) (primary-slot world handle))]
           (do (cells-set! (current-cells world) slot value)
               (mirror! world slot write-root!))
           (write-root! value)))
      value)
    (do (write-root! value) value)))

(defn sync-primary-root!
  "After a root write to `handle` that ran user code (a host atom's swap!)
   in a primary world: once that world is realized, make its slot agree with
   the current root (`read-root`). Each syncing writer re-reads the root after
   writing the slot, so the last one leaves them equal. The root write itself
   precedes this section; no JVM/CLJS path tracks a host atom."
  [handle read-root]
  (when-let [^ReadWorld active (active-world-state)]
    (let [^DenseWorld world (.-world active)]
      (when (and (.-primary? active) @(.-persistent? world))
        (when-let [slot (primary-slot world handle)]
          (with-cells-read-lock
            world
            #(loop []
               (let [r (read-root)]
                 (cells-set! (current-cells world) slot r)
                 (when-not (identical? r (read-root)) (recur)))))))))
  nil)

(defn reset-var-meta!
  ([handle m] (reset-var-meta! handle m nil))
  ([handle m mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [meta-slot]} (or (descriptor world handle)
                                   (allocate-descriptor! world handle :var
                                                         (not (:sci/built-in m))
                                                         (:sci.impl/fork-fn m)))]
       (reset-slot! world meta-slot m mirror))
     ::no-world)))

(defn reset-namespace-meta!
  ([handle m] (reset-namespace-meta! handle m nil))
  ([handle m mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (or (descriptor world handle)
               (allocate-descriptor! world handle :namespace false nil))]
       (reset-slot! world value-slot m mirror))
     ::no-world)))

(defn reset-type-data!
  ([handle data] (reset-type-data! handle data nil))
  ([handle data mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (or (descriptor world handle)
               (allocate-descriptor! world handle :type false nil))]
       (reset-slot! world value-slot data mirror))
     ::no-world)))

(defn- alter-slot!
  "Apply `f` (user code) outside every lock, then CAS the result in a slot
   section; retry when the slot changed meanwhile."
  [^DenseWorld world slot fallback f args mirror]
  (ensure-capacity! world (inc slot))
  (loop []
    (let [raw-old (cells-get (current-cells world) slot)
          old (if (identical? absent raw-old) fallback raw-old)
          new (apply f old args)]
      (if (with-cells-read-lock
            world
            #(when (cells-cas! (current-cells world) slot raw-old new)
               (mirror! world slot mirror)
               true))
        new
        (recur)))))

(defn alter-var-meta!
  ([handle fallback f args] (alter-var-meta! handle fallback f args nil))
  ([handle fallback f args mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [meta-slot]} (or (descriptor world handle)
                                   (allocate-descriptor! world handle :var
                                                         (not (:sci/built-in fallback))
                                                         (:sci.impl/fork-fn fallback)))]
       (alter-slot! world meta-slot fallback f args mirror))
     ::no-world)))

(defn alter-namespace-meta!
  ([handle fallback f args] (alter-namespace-meta! handle fallback f args nil))
  ([handle fallback f args mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (or (descriptor world handle)
               (allocate-descriptor! world handle :namespace false nil))]
       (alter-slot! world value-slot fallback f args mirror))
     ::no-world)))

(defn alter-type-data!
  ([handle fallback f args] (alter-type-data! handle fallback f args nil))
  ([handle fallback f args mirror]
   (if-let [^DenseWorld world (current-world)]
     (let [{:keys [value-slot]}
           (or (descriptor world handle)
               (allocate-descriptor! world handle :type false nil))]
       (alter-slot! world value-slot fallback f args mirror))
     ::no-world)))

(defn alter-var-watches! [handle fallback f args]
  (if-let [^DenseWorld world (current-world)]
    (let [{:keys [watch-slot]}
          (allocate-var-watch-slot! world handle fallback)]
      (alter-slot! world watch-slot fallback f args nil))
    ::no-world))

(defn swap-value!
  "Atomically update one tracked slot. Unrelated SCI atoms do not contend and
  cannot cause `f` to be retried. `f` and `validate!` run outside every lock."
  [handle f args validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))]
    (loop []
      (let [old (cells-get (current-cells world) slot)
            new (apply f old args)]
        (validate! new)
        (if (with-cells-read-lock
              world #(cells-cas! (current-cells world) slot old new))
          (do (notify! old new) new)
          (recur))))))

(defn reset-tracked! [handle new validate! notify!]
  (swap-value! handle (constantly new) nil validate! notify!))

(defn compare-and-set-tracked! [handle expected new validate! notify!]
  (let [^DenseWorld world (current-world)
        slot (:value-slot (descriptor world handle))
        old (cells-get (current-cells world) slot)]
    (if-not (identical? expected old)
      false
      (do
        (validate! new)
        (if (with-cells-read-lock
              world #(cells-cas! (current-cells world) slot old new))
          (do (notify! old new) true)
          false)))))

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

;; A primary world reads its host objects (Var roots, volatiles) directly
;; until its first fork. Every write through SCI to such a host object runs
;; in a slot section (with its slot once realized; a still-mutable volatile
;; root via `mutable-primary-write!`), so this realization, holding the
;; sections exclusively, reads a consistent set of roots. Only a host atom's
;; swap! (user code, not tracked on JVM/CLJS) writes its root outside one.
(defn- realize-primary! [^DenseWorld world snapshot-value snapshot-var-meta]
  (when-not @(.-persistent? world)
    (vreset! (.-persistent? world) true)
    (doseq [[handle {:keys [kind value-slot meta-slot]}]
            (:descriptors @(.-registry world))]
      (when (present-slot? world value-slot)
        (cells-set! (current-cells world) value-slot (snapshot-value handle))
        (when (= :var kind)
          (cells-set! (current-cells world) meta-slot (snapshot-var-meta handle))))))
  world)

(defn- call-with-selected-world
  ([world f] (call-with-selected-world world false f))
  ([^DenseWorld world primary? f]
   (let [state #?(:cljd (execution/current-state)
                  :clj (.get ^ThreadLocal execution/current)
                  :cljs (execution/current-state))
         previous (execution/active-world state)
         previous-scope (execution/binding-scope state)]
     (install-active-world! state (ReadWorld. world primary?))
     (execution/set-binding-scope! state world)
     (execution/refresh-active-bindings! state)
     (try
       (f)
       (finally
         (install-active-world! state previous)
         (execution/set-binding-scope! state previous-scope)
         (execution/refresh-active-bindings! state))))))

(defn- copy-cells!
  "Inside the copy section: the source's cells as a new unpublished world
   (sharing the lineage registry) and the registry state they were cut at."
  [^DenseWorld world]
  (sweep-managed! world)
  (let [source (current-cells world)
        registry-state @(.-registry world)
        logical-n (:next-slot registry-state)
        target (new-cells (max 16 logical-n))]
    (dotimes [i logical-n]
      ;; The registry is shared by siblings, but each world's physical array
      ;; only grows when that world allocates. Sibling-only slots are absent.
      (cells-set! target i (if (< i (cells-length source))
                            (cells-get source i)
                            absent)))
    ;; SCI's own descriptor fork fns (loaded-libs ref, CLJS type data,
    ;; ClojureDart method tables) snapshot SCI-owned mutable state, so they
    ;; run here, cut with the cells and the environment. They are internal
    ;; and never block or run user code. They establish target-local
    ;; realizations before host Forkable values see the child world.
    (doseq [[_ {:keys [value-slot value-fork-fn]}] (:descriptors registry-state)
            :when value-fork-fn
            :when (< value-slot logical-n)
            :let [v (cells-get target value-slot)]
            :when (not (identical? absent v))]
      (cells-set! target value-slot (value-fork-fn v)))
    [(dense-world target (.-registry world) true) registry-state
     ;; Which world-held Futures were done at the cut: the fork policy shares
     ;; those and forks the others as pending-at-the-cut (`sci.fork`).
     #?(:clj (let [done (IdentityHashMap.)]
               (dotimes [i logical-n]
                 (let [v (cells-get target i)]
                   (when (and (instance? java.util.concurrent.Future v)
                              ;; never an untrusted isDone: host code
                              (fork/trusted-future? v)
                              (.isDone ^java.util.concurrent.Future v))
                     (.put done v true))))
               done)
        :default nil)]))

(defn- fork-values!
  "After the copy section: host `Forkable`/`:fork-fn` processing of the
   unpublished target. Runs host code; holds no lock of the source."
  [^DenseWorld target-world registry-state fork-value done-at-cut]
  (binding [fork/*done-at-cut* done-at-cut]
  (let [target (current-cells target-world)
        logical-n (:next-slot registry-state)
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
                     :cljd (boolean (some #(identical? % value) handles))))]
    (call-with-selected-world
     target-world
     (fn []
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
         (cells-set! target slot (fork-value v))))))))

(defn fork-world
  "Fork `world`. The copy section excludes the source's slot writers and
   publications (never its evaluations, which may include the caller's own):
   it realizes a primary, copies the cells and the lazy realization caches
   and calls `snapshot-extra` (the namespace environment), so all of them are
   cut at one point. Host fork processing of the unpublished target runs
   afterwards. `in-source` runs a thunk with the source's context bound
   (primary realization reads host objects through it). Returns
   [target-world extra]."
  ([world fork-value snapshot-value snapshot-var-meta]
   (first (fork-world world fork-value snapshot-value snapshot-var-meta
                      (fn [f] (f)) (constantly nil))))
  ([^DenseWorld world fork-value snapshot-value snapshot-var-meta in-source snapshot-extra]
   (assert-not-copying! world :fork)
   (assert-open! world)
   (let [target (volatile! nil)]
     (try
       (with-copying
        world
        (fn []
          (let [[target-world registry-state extra done-at-cut]
             (with-copy-section
               world
               (fn []
                 (assert-open! world)
                 (in-source
                  #(call-with-selected-world
                    world true
                    (fn [] (realize-primary! world snapshot-value snapshot-var-meta))))
                 (let [[target-world registry-state done-at-cut] (copy-cells! world)]
                   (vreset! target target-world)
                   ;; Anonymous cache owners participate without reserving a
                   ;; dense slot per lazy node. Their snapshots never invoke
                   ;; deferred computations, block or run user code.
                   (call-with-selected-world
                    target-world
                    (fn []
                      (doseq [owner (world-cache-owners world)]
                        (-fork-world-cache! owner world target-world))))
                   [target-world registry-state (snapshot-extra) done-at-cut])))]
         (when fork-value
           (fork-values! target-world registry-state fork-value done-at-cut))
         [target-world extra])))
       (catch #?(:clj Throwable :cljs :default :cljd Object) error
         (when-let [target-world @target]
           (try
             (retire-world! target-world)
             (catch #?(:clj Throwable :cljs :default :cljd Object) cleanup-error
               (throw (fork-cleanup-failure error cleanup-error target-world nil)))))
         (throw error))))))
