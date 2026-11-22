(ns sci.lang
  (:require [sci.ctx-store]
            [sci.impl.types :as types]
            [sci.impl.vars :as vars]
            [sci.impl.world :as world])
  (:refer-clojure :exclude [Var ->Var var? Namespace ->Namespace]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(defprotocol IWorldMirror
  "Raw writes of the host object a primary world reads directly. Only called
   inside a world slot section (`sci.impl.world/mirror!`): no user code, no
   locks."
  (-mirror-root! [this v])
  (-mirror-meta! [this m]))

(defn- primary-mirror-root [x]
  (when (world/primary-world?) #(-mirror-root! x %)))

(defn- primary-mirror-meta [x]
  (when (world/primary-world?) #(-mirror-meta! x %)))

;; marker interface for vars, clj only for now
#?(:cljd nil :clj (definterface ^{:doc "Marker interface for SCI vars."} IVar))

(deftype ^{:doc "Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly."}
    Type [#?(:cljd ^:mutable data :clj ^:volatile-mutable data :cljs ^:volatile-mutable data)
          ;; an atom of the home, the world that defined the type: [its
          ;; context, its ReadWorld], true when defined in unrelated
          ;; lineages, nil if never defined in a world
          home]
  world/IWorldTracked
  (-world-tracked? [_] (some? @home))
  (-world-home-ctx [_] (let [h @home] (when (vector? h) (nth h 0))))
  (-mark-world-tracked! [_ home-ctx]
    ;; Elected by CAS: the first definer wins; a definition in an unrelated
    ;; lineage leaves no unambiguous home (reads fall back to the host field).
    (loop []
      (let [current @home]
        (cond
          (nil? current)
          (when-not (compare-and-set! home nil
                                      (if home-ctx
                                        [home-ctx (:sci.impl/read-world home-ctx)]
                                        true))
            (recur))
          (and (vector? current) home-ctx
               (not (identical? (:sci.impl/lineage (nth current 0))
                                (:sci.impl/lineage home-ctx))))
          (when-not (compare-and-set! home current true) (recur)))))
    true)
  world/ITypeData
  (-data-for [this]
    ;; the hot path of type methods: one read of the home; a world's own
    ;; slot counts also before a home is elected (an imported type, a
    ;; definition between its slot write and its election)
    (let [h @home]
      (world/type-data-at this (when (vector? h) (nth h 1)) true data)))
  (-own-data [this clone]
    (world/own-type-data this data clone))
  (-own-data! [this clone]
    (let [d data]
      (world/own-type-data! this #(world/type-data-of this true d) clone
                            (primary-mirror-root this))))
  IWorldMirror
  (-mirror-root! [_ v] (set! data v))
  (-mirror-meta! [_ m] (set! data m))
  sci.impl.types/IBox
  (getVal [this] (let [h @home] (world/type-data-at this (when (vector? h) (nth h 1)) false data)))
  (setVal [this v]
    (if (world/current-world)
      (do
        (world/reset-type-data! this v (primary-mirror-root this))
        v)
      (set! data v)))
  Object
  ;; NOTE: returns "user.Foo" rather than "class user.Foo" (unlike java.lang.Class).
  ;; Changing this would break downstream libs (e.g. prismatic/schema).
  (toString [this]
    ;; The name never changes and the host field always has it, so printing
    ;; a type works without its world (also once that world is retired).
    (str (or (:sci.impl/type-name data)
             (:sci.impl/type-name (let [h @home] (world/type-data-at this (when (vector? h) (nth h 1)) false data))))))

  ;; meta is only supported to get our implementation! keys out
  #?@(:cljd
      [IMeta
       (-meta [this] (let [h @home] (world/type-data-at this (when (vector? h) (nth h 1)) false data)))]
      :clj
      [clojure.lang.IMeta
       (meta [this] (let [h @home] (world/type-data-at this (when (vector? h) (nth h 1)) false data)))]
      :cljs
      [IMeta
       (-meta [this] (let [h @home] (world/type-data-at this (when (vector? h) (nth h 1)) false data)))])

  ;; support alter-meta! for storing print-method etc.
  #?@(:cljd
      [types/IResetMeta
       (-reset-meta! [this m]
         (if (world/current-world)
           (do
             (world/reset-type-data! this m (primary-mirror-root this))
             m)
           (set! data m)))]
      :clj
      [clojure.lang.IReference
       ;; In a world `f` runs outside every world lock (the monitor only
       ;; orders alterMeta callers); resetMeta takes no monitor there, as it
       ;; may run inside a publication.
       (alterMeta [this f args]
                  (locking this
                    (if (world/current-world)
                      (world/alter-type-data!
                       this (world/type-data-of this true data) f args
                       (primary-mirror-root this))
                      (set! data (apply f data args)))))
       (resetMeta [this m]
                  (if (world/current-world)
                    (do
                      (world/reset-type-data! this m (primary-mirror-root this))
                      m)
                    (locking this (set! data m))))])

  types/HasName
  (getName [this] (str this)))

#?(:cljd nil
   :clj (defmethod print-method Type [this w]
          (.write ^java.io.Writer w (str this))))

(defn- throw-root-binding [this]
  (throw #?(:cljd (ex-info (str "Can't change/establish root binding of " this " with set") {})
            :clj (IllegalStateException.
                  (str "Can't change/establish root binding of " this " with set"))
            :cljs (js/Error.
                   (str "Can't change/establish root binding of " this " with set")))))

(defn notify-watches [ref watches old-val new-val]
  (when watches
    (when (pos? (count watches))
      ;; Watches are user code: inside a publication (a def) they run after it.
      (world/run-after-publication!
       #(reduce-kv (fn [_ k f]
                     (f k ref old-val new-val)
                     nil)
                   nil
                   watches))))
  ref)

(deftype ^{:doc "Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly."}
    Var [#?(:cljd ^:mutable root
            :clj ^:volatile-mutable root
            :cljs ^:mutable root)
         sym
         #?(:cljd ^:mutable meta
            :clj ^:volatile-mutable meta
            :cljs ^:mutable meta)
         #?(:cljd ^:mutable thread-bound
            :clj ^:volatile-mutable thread-bound
            :cljs ^:mutable thread-bound)
         #?(:cljd ^:mutable needs-ctx
            :clj ^:volatile-mutable needs-ctx
            :cljs ^:mutable needs-ctx)
         #?(:cljd ^:mutable watches
            :clj ^:volatile-mutable watches
            :cljs ^:mutable watches)
         ns
         #?(:cljd ^:mutable world-tracked
            :clj ^:volatile-mutable world-tracked
            :cljs ^:mutable world-tracked)
         ;; trusted `:sci/built-in` (`sci.impl.world/IBuiltIn`), never read
         ;; from the metadata
         #?(:cljd ^:mutable built-in
            :clj ^:volatile-mutable built-in
            :cljs ^:mutable built-in)]
  ;; marker interface, clj only for now
  #?@(:cljd [] :clj [sci.lang.IVar])
  world/IBuiltIn
  (-built-in? [_] (boolean built-in))
  (-set-built-in! [this b]
    #?(:cljd (set! built-in (boolean b))
       :default (set! (.-built-in this) (boolean b))))
  IWorldMirror
  (-mirror-root! [_ v] (vars/bumping-set! root v))
  (-mirror-meta! [_ m] (set! meta m))
  world/IWorldTracked
  (-world-tracked? [_] (boolean world-tracked))
  (-world-home-ctx [_] (when (map? world-tracked) world-tracked))
  (-mark-world-tracked! [this home-ctx]
    (let [current world-tracked]
      (when (and (map? current)
                 home-ctx
                 (not (identical? (:sci.impl/lineage current)
                                  (:sci.impl/lineage home-ctx))))
        ;; A stable host/built-in Var can be installed in unrelated contexts.
        ;; It then has no unambiguous implicit home; callers select a world
        ;; explicitly with sci/call-with-context.
        #?(:cljd (set! world-tracked true)
           :default (set! (.-world-tracked this) true)))
      (when-not current
        #?(:cljd (set! world-tracked (or home-ctx true))
           :default (set! (.-world-tracked this) (or home-ctx true)))))
    true)
  types/HasName
  (getName [this]
    (or (:name (if world-tracked (world/var-meta this meta) meta)) sym))
  vars/IVar
  (bindRoot [this v]
    (let [old-root (if world-tracked (world/var-value this root) root)
          current-meta (if world-tracked (world/var-meta this meta) meta)]
      (vars/with-writeable-var this current-meta
        (if (world/current-world)
          (world/register-var! this v current-meta nil (primary-mirror-root this))
          (vars/bumping-set! root v)))
      (notify-watches this (world/var-watches this watches) old-root v))
    ;; this is the return value for alter-var-root which should be the only place calling bindRoot directly
    v)
  (getRawRoot [this]
    (if world-tracked (world/var-value this root) root))
  (getRawWatches [_] watches)
  (getDirectRoot [this]
    (if thread-bound
      (if-let [tbox (vars/get-thread-binding this)]
        (types/getVal tbox)
        (if world-tracked (world/var-value this root) root))
      (if world-tracked (world/var-value this root) root)))
  (selectRoot [this world-value]
    (if thread-bound
      (if-let [tbox (vars/get-thread-binding this)]
        (types/getVal tbox)
        world-value)
      world-value))
  (getRootAt [this slot]
    ;; A Var can be marked ^:dynamic after this read site was analyzed. Keep
    ;; the same late binding-frame check as deref while retaining the resolved
    ;; root slot for the common non-dynamic case.
    (let [x (if thread-bound
              (if-let [tbox (vars/get-thread-binding this)]
                (types/getVal tbox)
                (world/var-value-at slot root))
              (world/var-value-at slot root))]
      (if (identical? x world/absent)
        ;; no realization in the active world: the full read consults the
        ;; Var's home (its value, or the retired error)
        (world/var-value this root)
        x)))
  (toSymbol [this]
    ;; if we have at least a name from metadata, then build the symbol from that
    (let [current-meta (if world-tracked (world/var-meta this meta) meta)]
      (if-let [sym-name (some-> (:name current-meta) name)]
        (symbol (some-> (:ns current-meta) types/getName name) sym-name)
      ;; otherwise, fall back to the symbol
        sym)))
  (isMacro [this]
    (let [current-meta (if world-tracked (world/var-meta this meta) meta)
          current-root (if world-tracked (world/var-value this root) root)]
      (or (:macro current-meta)
          (when-some [m (clojure.core/meta current-root)]
            (:sci/macro m)))))
  (setThreadBound [this v]
    #?(:cljd (set! thread-bound v)
       :default (set! (.-thread-bound this) v)))
  (unbind [this]
    (let [current-meta (if world-tracked (world/var-meta this meta) meta)]
      (vars/with-writeable-var this current-meta
        (if (world/current-world)
          (let [unbound (vars/->SciUnbound this)]
            ;; A newly interned Var has no slot yet, especially in a child
            ;; world. Registering here preserves unboundness without mutating
            ;; the shared direct root.
            (world/register-var! this unbound current-meta nil
                                 (primary-mirror-root this)))
          (vars/bumping-set! (.-root this) (vars/->SciUnbound this))))))
  (hasRoot [this]
    (not (instance? #?(:cljd vars/SciUnbound
                       :clj sci.impl.vars.SciUnbound
                       :cljs sci.impl.vars.SciUnbound)
                    (if world-tracked (world/var-value this root) root))))
  vars/DynVar
  (dynamic? [this]
    (:dynamic (if world-tracked (world/var-meta this meta) meta)))
  types/IBox
  (setVal [this v]
    (if-let [b (vars/get-thread-binding this)]
      #?(:cljd (types/setVal b v)
         :clj
         (let [t (.-thread b)]
           (if (not (identical? t (Thread/currentThread)))
             (throw (IllegalStateException.
                     (format "Can't set!: %s from non-binding thread" (vars/toSymbol this))))
             (types/setVal b v)))
         :cljs (types/setVal b v))
      #?(:cljd (if (:unrestricted sci.ctx-store/*ctx*)
                 (if (world/current-world)
                   (world/reset-value! this v (primary-mirror-root this))
                   (set! (.-root this) v))
                 (throw-root-binding this))
         :clj (throw-root-binding this)
         :cljs (if (:unrestricted sci.ctx-store/*ctx*)
                 (if (world/current-world)
                   (world/reset-value! this v (primary-mirror-root this))
                   (vars/bumping-set! (.-root this) v))
                 (throw-root-binding this)))))
  (getVal [this] (if world-tracked (world/var-value this root) root))
  #?(:cljd IDeref :clj clojure.lang.IDeref :cljs IDeref)
  (#?(:cljd -deref
      :clj deref
      :cljs -deref) [this]
    (if thread-bound
      (if-let [tbox (vars/get-thread-binding this)]
        (types/getVal tbox)
        (if world-tracked (world/var-value this root) root))
      (if world-tracked (world/var-value this root) root)))
  Object
  (toString [this]
    (str "#'" (vars/toSymbol this)))
  #?(:cljs IPrintWithWriter)
  #?(:cljs (-pr-writer [a writer opts]
                       (-write writer "#'")
                       (-pr-writer (vars/toSymbol a) writer opts)))
  #?(:cljd IMeta :clj clojure.lang.IMeta :cljs IMeta)
  ;; `:sci/built-in` as the trusted flag says (`vars/flag-meta`), in every
  ;; world, also one whose metadata predates the flag
  #?(:cljd (-meta [this] (vars/flag-meta (if world-tracked (world/var-meta this meta) meta) (world/-built-in? this)))
     :clj (clojure.core/meta [this] (vars/flag-meta (if world-tracked (world/var-meta this meta) meta) (world/-built-in? this)))
     :cljs (-meta [this] (vars/flag-meta (if world-tracked (world/var-meta this meta) meta) (world/-built-in? this))))
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  #?@(:cljd [] :clj [clojure.lang.IReference
                     (alterMeta [this f args]
                                ;; provenance before the Var's context is selected
                                (let [source (vars/meta-write-source)
                                      f (vars/meta-fn this f source)]
                                  (world/call-with-var-context
                                   this
                                   (fn []
                                     (let [current-meta (world/var-meta this meta)]
                                       (vars/with-writeable-var this current-meta
                                         ;; In a world `f` runs outside every
                                         ;; world lock; the monitor only orders
                                         ;; alterMeta callers.
                                         (locking this
                                           (vars/commit-built-in!
                                            this
                                            (if (world/current-world)
                                              (world/alter-var-meta! this current-meta f args
                                                                     (primary-mirror-meta this))
                                              (set! (.-meta this) (apply f meta args)))
                                            source))))))))
                     (resetMeta [this m]
                                (let [source (vars/meta-write-source)
                                      m (vars/meta-to-write this m source)]
                                  (world/call-with-var-context
                                   this
                                   (fn []
                                     (let [current-meta (world/var-meta this meta)]
                                       (vars/with-writeable-var this current-meta
                                         ;; No monitor in a world: this may run
                                         ;; inside a publication (def).
                                         (if (world/current-world)
                                           (do (world/reset-var-meta! this m (primary-mirror-meta this))
                                               (vars/commit-built-in! this m source))
                                           (locking this
                                             (vars/commit-built-in! this (set! (.-meta this) m) source)))))))))])
  #?@(:cljd [types/IResetMeta
             (-reset-meta! [this m]
               (let [source (vars/meta-write-source)
                     m (vars/meta-to-write this m source)]
                 (world/call-with-var-context
                  this
                  (fn []
                    (let [current-meta (world/var-meta this meta)]
                      (vars/with-writeable-var this current-meta
                        (vars/commit-built-in!
                         this
                         (if (world/current-world)
                           (do (world/reset-var-meta! this m (primary-mirror-meta this))
                               m)
                           (set! (.-meta this) m))
                         source)))))))
             IWatchable
             (-add-watch [this key watch-fn]
                         (world/call-with-var-context
                          this
                          (fn []
                            (let [current-meta (world/var-meta this meta)]
                              (vars/with-writeable-var this current-meta
                                (if (world/current-world)
                                  (do
                                    (when-not (world/tracked? this)
                                      (world/register-var!
                                       this root current-meta watches))
                                    (world/alter-var-watches!
                                     this watches assoc [key watch-fn]))
                                  (set! (.-watches this) (assoc watches key watch-fn)))))))
                         this)
             (-remove-watch [this key]
                            (world/call-with-var-context
                             this
                             (fn []
                               (let [current-meta (world/var-meta this meta)]
                                 (vars/with-writeable-var this current-meta
                                   (if (world/current-world)
                                     (do
                                       (when-not (world/tracked? this)
                                         (world/register-var!
                                          this root current-meta watches))
                                       (world/alter-var-watches!
                                        this watches dissoc [key]))
                                     (set! (.-watches this) (dissoc watches key)))))))
                            this)]
      :clj [clojure.lang.IRef
            (addWatch [this key watch-fn]
                      (world/call-with-var-context
                       this
                       (fn []
                         (let [current-meta (world/var-meta this meta)]
                           (vars/with-writeable-var this current-meta
                             (if (world/current-world)
                               (do
                                 (when-not (world/tracked? this)
                                   (world/register-var!
                                    this root current-meta watches))
                                 (world/alter-var-watches!
                                  this watches assoc [key watch-fn]))
                               (set! (.-watches this) (assoc watches key watch-fn)))))))
                      this)
            (removeWatch [this key]
                         (world/call-with-var-context
                          this
                          (fn []
                            (let [current-meta (world/var-meta this meta)]
                              (vars/with-writeable-var this current-meta
                                (if (world/current-world)
                                  (do
                                    (when-not (world/tracked? this)
                                      (world/register-var!
                                       this root current-meta watches))
                                    (world/alter-var-watches!
                                     this watches dissoc [key]))
                                  (set! (.-watches this) (dissoc watches key)))))))
                         this)]
      :cljs [IWatchable
            (-add-watch [this key watch-fn]
                        (world/call-with-var-context
                         this
                         (fn []
                           (let [current-meta (world/var-meta this meta)]
                             (vars/with-writeable-var this current-meta
                               (if (world/current-world)
                                 (do
                                   (when-not (world/tracked? this)
                                     (world/register-var!
                                      this root current-meta watches))
                                   (world/alter-var-watches!
                                    this watches assoc [key watch-fn]))
                                 (set! (.-watches this) (assoc watches key watch-fn)))))))
                        this)
            (-remove-watch [this key]
                           (world/call-with-var-context
                            this
                            (fn []
                              (let [current-meta (world/var-meta this meta)]
                                (vars/with-writeable-var this current-meta
                                  (if (world/current-world)
                                    (do
                                      (when-not (world/tracked? this)
                                        (world/register-var!
                                         this root current-meta watches))
                                      (world/alter-var-watches!
                                       this watches dissoc [key]))
                                    (set! (.-watches this) (dissoc watches key)))))))
                           this)])
  ;; #?(:cljs Fn) ;; In the real CLJS this is there... why?
  #?(:cljd IFn :clj clojure.lang.IFn :cljs IFn)
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this]
    ((vars/getDirectRoot this)))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a]
    ((vars/getDirectRoot this) a))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b]
    ((vars/getDirectRoot this) a b))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c]
    ((vars/getDirectRoot this) a b c))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d]
    ((vars/getDirectRoot this) a b c d))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e]
    ((vars/getDirectRoot this) a b c d e))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f]
    ((vars/getDirectRoot this) a b c d e f))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g]
    ((vars/getDirectRoot this) a b c d e f g))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h]
    ((vars/getDirectRoot this) a b c d e f g h))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i]
    ((vars/getDirectRoot this) a b c d e f g h i))
  #?@(:cljd
      [(-invoke-more [this a b c d e f g h i rest]
         (apply (vars/getDirectRoot this) a b c d e f g h i rest))
       (-apply [this args]
         (apply (vars/getDirectRoot this) args))]
      :clj
      [(invoke [this a b c d e f g h i j]
         ((vars/getDirectRoot this) a b c d e f g h i j))
       (invoke [this a b c d e f g h i j k]
         ((vars/getDirectRoot this) a b c d e f g h i j k))
       (invoke [this a b c d e f g h i j k l]
         ((vars/getDirectRoot this) a b c d e f g h i j k l))
       (invoke [this a b c d e f g h i j k l m]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m))
       (invoke [this a b c d e f g h i j k l m n]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n))
       (invoke [this a b c d e f g h i j k l m n o]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o))
       (invoke [this a b c d e f g h i j k l m n o p]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p))
       (invoke [this a b c d e f g h i j k l m n o p q]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q))
       (invoke [this a b c d e f g h i j k l m n o p q r]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s t))
       (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
         (apply (vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s t rest))
       (applyTo [this args]
                (apply (vars/getDirectRoot this) args))]
      :cljs
      [(-invoke [this a b c d e f g h i j]
         ((vars/getDirectRoot this) a b c d e f g h i j))
       (-invoke [this a b c d e f g h i j k]
         ((vars/getDirectRoot this) a b c d e f g h i j k))
       (-invoke [this a b c d e f g h i j k l]
         ((vars/getDirectRoot this) a b c d e f g h i j k l))
       (-invoke [this a b c d e f g h i j k l m]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m))
       (-invoke [this a b c d e f g h i j k l m n]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n))
       (-invoke [this a b c d e f g h i j k l m n o]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o))
       (-invoke [this a b c d e f g h i j k l m n o p]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p))
       (-invoke [this a b c d e f g h i j k l m n o p q]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
         ((vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s t))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
         (apply (vars/getDirectRoot this) a b c d e f g h i j k l m n o p q r s t rest))]))

#?(:cljd nil
   :clj
   ;; Use public interface for print-method so it can be overriden in bb itself
   (do (defmethod print-method sci.lang.IVar [o ^java.io.Writer w]
         (.write w (str "#'" (vars/toSymbol ^sci.impl.vars.IVar o))))
       (prefer-method print-method sci.lang.IVar clojure.lang.IDeref)))

(deftype
    ^{:doc
      "Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly."}
    Namespace [name #?(:cljd ^:mutable meta
                       :clj ^:volatile-mutable meta
                       :cljs ^:mutable meta)
               ;; trusted `:sci/built-in` (`sci.impl.world/IBuiltIn`)
               #?(:cljd ^:mutable built-in
                  :clj ^:volatile-mutable built-in
                  :cljs ^:mutable built-in)]
  world/IBuiltIn
  (-built-in? [_] (boolean built-in))
  (-set-built-in! [this b]
    #?(:cljd (set! built-in (boolean b))
       :default (set! (.-built-in this) (boolean b))))
  Object
  (toString [_]
    (str name))
  types/HasName
  (getName [_] name)
  IWorldMirror
  (-mirror-root! [_ m] (set! meta m))
  (-mirror-meta! [_ m] (set! meta m))
  #?(:cljd IMeta :clj clojure.lang.IMeta :cljs IMeta)
  #?(:cljd (-meta [this] (vars/flag-meta (world/namespace-meta this meta) (world/-built-in? this)))
     :clj (clojure.core/meta [this] (vars/flag-meta (world/namespace-meta this meta) (world/-built-in? this)))
     :cljs (-meta [this] (vars/flag-meta (world/namespace-meta this meta) (world/-built-in? this))))
  #?@(:cljd [types/IResetMeta
             (-reset-meta! [this m]
               (let [source (vars/meta-write-source)
                     m (vars/meta-to-write this m source)
                     current-meta (world/namespace-meta this meta)]
                 (vars/with-writeable-namespace this current-meta
                   (vars/commit-built-in!
                    this
                    (if (world/current-world)
                      (do (world/reset-namespace-meta! this m (primary-mirror-meta this))
                          m)
                      (set! meta m))
                    source))))]
      :clj [clojure.lang.IReference
            (alterMeta [this f args]
                       (let [source (vars/meta-write-source)
                             f (vars/meta-fn this f source)
                             current-meta (world/namespace-meta this meta)]
                         (vars/with-writeable-namespace this current-meta
                           (locking this
                             (vars/commit-built-in!
                              this
                              (if (world/current-world)
                                (world/alter-namespace-meta!
                                 this current-meta f args (primary-mirror-meta this))
                                (set! meta (apply f meta args)))
                              source)))))
            (resetMeta [this m]
                       (let [source (vars/meta-write-source)
                             m (vars/meta-to-write this m source)
                             current-meta (world/namespace-meta this meta)]
                         (vars/with-writeable-namespace this current-meta
                           (if (world/current-world)
                             (do (world/reset-namespace-meta! this m (primary-mirror-meta this))
                                 (vars/commit-built-in! this m source))
                             (locking this (vars/commit-built-in! this (set! meta m) source))))))]))

(defn new-type
  "A SCI type with type data `data` and no home yet."
  [data]
  (->Type data (atom nil)))
