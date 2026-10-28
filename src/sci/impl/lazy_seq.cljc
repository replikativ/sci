(ns sci.impl.lazy-seq
  "Interpreter-owned lazy realization. Logical sequence owners are weakly
   indexed by their lineage; each owner holds an independent cache per world.
   Native lazy/iterator caches never become implicitly shared managed state."
  {:no-doc true}
  (:refer-clojure :exclude [lazy-seq])
  (:require [sci.ctx-store :as store]
            [sci.fork :as fork]
            [sci.impl.world :as world]
            #?@(:cljd []
                :clj [[sci.impl.lazy-seq-macros :refer [def-sequence-type]]]))
  #?(:cljs (:require-macros [sci.impl.lazy-seq-macros :refer [def-sequence-type]]))
  #?@(:cljd []
      :clj [(:import [java.lang.ref WeakReference]
                     [java.util WeakHashMap Map List]
                     [clojure.lang ISeq SeqIterator IPending Murmur3])]))

(defn- explicit-forkable? [value]
  #?(:clj (or (instance? sci.fork.Forkable value)
               (and (seq (:impls fork/Forkable)) (satisfies? fork/Forkable value)))
     :default (satisfies? fork/Forkable value)))

(defn checked-input
  "Reject a directly supplied opaque sequence/resource before a managed
   producer can hide it in a host closure. Nested host graphs still require
   explicit host cooperation."
  [value]
  ;; An explicit adapter must short-circuit resource probes such as isDone.
  (when-not (explicit-forkable? value)
    (when-let [kind (fork/affine-resource-kind value)]
      (throw (ex-info "Foreign lazy or consuming sequence requires an explicit forkable adapter."
                      {:type ::foreign-sequence :resource-kind kind}))))
  value)

(defprotocol ISequenceData
  (-seq-context [this])
  (-seq-project [this])
  (-seq-data [this])
  (-seq-metadata [this])
  (-seq-meta-clone [this m])
  (-seq-realized? [this]))

(defn- sequence-data? [value]
  #?(:clj (or (instance? sci.impl.lazy_seq.ISequenceData value)
               ;; Protocol extensions remain supported and are observed live.
               ;; Without extensions, a negative satisfies? walks/allocates the
               ;; complete Java superclass graph on every native sequence tail.
               (and (seq (:impls ISequenceData)) (satisfies? ISequenceData value)))
     :default (satisfies? ISequenceData value)))

(declare lazy-seq* contextualize sequence-view seq-value seq-first seq-next
         seq-rest seq-count seq-equiv seq-hash seq-java-hash seq-meta-clone
         seq-realized? seq-string java-list seq-iterator chunk-first* chunk-next*
         chunk-rest* owner-context owner-value normalize-seq make-owner projection-data!
         seq-reduce)

(def ^:private realization-requires-normalization?
  ;; LazySeq's pending marker changed from its thunk to its normalization
  ;; lock on newer JVM runtimes. Probe that observable contract rather than
  ;; tying interpreter behavior to a version string. Use reusable thunks:
  ;; core/lazy-seq's :once closure can clear captured fields on a failed call.
  #?(:cljd false
     :clj
     (let [probe (clojure.lang.LazySeq.
                  (fn []
                    (clojure.lang.LazySeq.
                     (fn [] (throw (ex-info "Lazy normalization probe" {}))))))]
       (try (seq probe) (catch clojure.lang.ExceptionInfo _ nil))
       (not (realized? probe)))
     :default false))


#?(:cljd
   (do
     (defn lazy-seq* [_]
       (throw (ex-info "Managed lazy realization requires the JVM or JavaScript weak-owner runtime."
                       {:type ::unsupported-runtime})))
     (defn contextualize
       ([_ value] value)
       ([_ value _project] value)))
   :default
   (do
     (deftype SeqBox [state projected gate])

     (defn- box [state]
       (SeqBox. (volatile! state) (volatile! nil) #?(:clj (Object.) :cljs nil)))

     (defn- with-box [^SeqBox box f]
       #?(:clj (locking (.-gate box) (f)) :cljs (f)))

     (defn- weak [value]
       #?(:clj (WeakReference. value) :cljs (js/WeakRef. value)))

     (defn- weak-value [reference]
       #?(:clj (.get ^WeakReference reference) :cljs (.deref reference)))

     (defn- cache-get [caches selected]
       #?(:clj (locking caches (.get ^Map caches selected))
          :cljs (.get caches selected)))

     (defn- cache-put! [caches selected value]
       #?(:clj (locking caches (.put ^Map caches selected value))
          :cljs (.set caches selected value))
       value)

     (deftype Owner [home caches lineage projection-source]
       world/IForkableWorldCache
       (-fork-world-cache! [_ source target]
         (when-let [^SeqBox source-box (cache-get caches source)]
           ;; Source evaluation is quiescent. Copy realization state without
           ;; invoking its thunk, normalizing it, or sharing its lock.
           (cache-put! caches target
                       (with-box source-box #(box @(.-state source-box))))))
       world/IWorldCache
       (-retire-world-cache! [_ selected]
         #?(:clj (locking caches (.remove ^Map caches selected))
            :cljs (.delete caches selected))
         nil))

     (defn- owner-context [^Owner owner preferred]
       (let [current store/*ctx*
             home (or preferred (weak-value (.-home owner)))]
         (or (when (and current (identical? (:sci.impl/lineage current) (.-lineage owner))) current)
             home
             (throw (ex-info "Lazy realization requires its live SCI context."
                             {:type ::missing-context})))))

     (defn- owner-box [^Owner owner]
       (or (cache-get (.-caches owner) (world/selected-world))
           (throw (ex-info "Lazy value has no realization in the selected SCI world."
                           {:type ::foreign-world}))))

     (defn- make-owner
       ([state] (make-owner state nil))
       ([state projection-source]
        (let [ctx (store/get-ctx)
              selected (world/selected-world)]
          (when-not (and selected (:sci.impl/world ctx))
            (throw (ex-info "Managed lazy values must be created inside a forkable SCI context."
                            {:type ::missing-context})))
          (world/assert-open! selected)
          (let [owner (Owner. (weak ctx) #?(:clj (WeakHashMap.) :cljs (js/WeakMap.))
                              (:sci.impl/lineage ctx) projection-source)]
            (cache-put! (.-caches owner) selected (box state))
            (world/register-world-cache! owner)
            owner))))

     (declare ->ManagedSeq ->ManagedChunkedSeq)

     (def-sequence-type ManagedSeq [owner ctx value metadata project]
       ((-seq-context [_]
          (if owner (owner-context owner ctx) (world/active-ctx ctx)))
        (-seq-project [_] project)
        (-seq-data [_]
          (if owner
            (if project (projection-data! owner project) (owner-value owner))
            (normalize-seq value)))
        (-seq-metadata [_] metadata)
        (-seq-meta-clone [this m]
          (if (identical? metadata m)
            this
            (if owner
              #?(:clj (do (owner-value owner)
                          (->ManagedSeq owner ctx nil m project))
                 :cljs (->ManagedSeq
                        (make-owner {:phase :pending :thunk #(owner-value owner)} owner)
                        ctx nil m project))
              (sequence-view (store/get-ctx) (with-meta value m)))))
        (-seq-realized? [_]
          (if owner
            (let [^SeqBox b (owner-box owner)]
              (with-box b #(if realization-requires-normalization?
                             (= :ready (:phase @(.-state b)))
                             (not= :pending (:phase @(.-state b))))))
            (if #?(:clj (instance? IPending value) :cljs (satisfies? IPending value))
              (realized? value) true))))
       false)

     (def-sequence-type ManagedChunkedSeq [delegate]
       ((-seq-context [_] (-seq-context delegate))
        (-seq-project [_] (-seq-project delegate))
        (-seq-data [_] (-seq-data delegate))
        (-seq-metadata [_] (-seq-metadata delegate))
        (-seq-meta-clone [_ m] (-seq-meta-clone delegate m))
        (-seq-realized? [_] (-seq-realized? delegate)))
       true)

     (defn- call-seq [value f]
       (world/call-with-context (-seq-context value) f))

     (defn- interrupt! []
       (when-let [f (:interrupt-fn (store/get-ctx))] (f)))

     (defn- raw-value! [^Owner owner]
       (let [^SeqBox b (owner-box owner)]
         (with-box b
           (fn []
             (interrupt!)
             (let [state @(.-state b)]
               (if (= :pending (:phase state))
                 (let [value ((:thunk state))]
                   ;; Invocation failures leave the thunk retryable. Clear it
                   ;; only after it returns, matching LazySeq rather than Delay.
                   (vreset! (.-state b) {:phase :raw :value value})
                   value)
                 (:value state)))))))

     (defn- source-value [value]
       (if (instance? ManagedSeq value)
         (let [^ManagedSeq value value]
           (if (.-owner value) value (.-value value)))
         (source-value (.-delegate ^ManagedChunkedSeq value))))

     (defn- normalize-seq [initial]
       ;; Native LazySeq unwraps nested lazy results iteratively. In particular
       ;; rejected filter elements must not consume a JVM stack frame each.
       (loop [value initial]
         (interrupt!)
         (cond
           (and (instance? ManagedSeq value) (.-owner ^ManagedSeq value))
           (let [owner (.-owner ^ManagedSeq value)
                 selected (-seq-context value)]
             (if (identical? (:sci.impl/world selected) (world/selected-world))
               ;; Public context projection is an observation cache, never
               ;; logical program state. A lazy body may return a public view;
               ;; normalize its raw owner so later forks re-project locally.
               (recur (raw-value! owner))
               (call-seq value #(normalize-seq (raw-value! owner)))))

           (sequence-data? value)
           (let [selected (-seq-context value)]
             (if (identical? (:sci.impl/world selected) (world/selected-world))
               (recur (source-value value))
               (call-seq value #(normalize-seq (source-value value)))))

           :else
           (let [s (seq (checked-input value))]
             (if (sequence-data? s)
               (recur s)
               (checked-input s))))))

     (defn- owner-value [owner]
       (let [^SeqBox b (owner-box owner)]
         (with-box b
           (fn []
             (interrupt!)
             (let [state @(.-state b)]
               (if (= :ready (:phase state))
                 (:value state)
                 (let [raw (raw-value! owner)]
                   ;; JVM LazySeq clears its intermediate result before seq
                   ;; normalization. Newer JVMs retain a pending marker until
                   ;; normalization succeeds; both abandon the raw result on
                   ;; failure. CLJS retains its raw result instead.
                   #?(:clj (vreset! (.-state b)
                                    {:phase (if realization-requires-normalization?
                                              :normalizing :ready)
                                     :value nil}))
                   (let [result (normalize-seq raw)]
                     (vreset! (.-state b) {:phase :ready :value result})
                     result))))))))

     (defn lazy-seq*
       "Build a managed, still-unrealized sequence in the selected SCI world."
       [thunk]
       (->ManagedSeq (make-owner {:phase :pending :thunk thunk}) nil nil nil nil))

     (defn- sequence-view
       [ctx value]
       (when-not (nil? value)
         (let [delegate (->ManagedSeq nil ctx value (meta value) nil)]
           (if (chunked-seq? value) (->ManagedChunkedSeq delegate) delegate))))

     (defn contextualize
       "Bind a returned seq to its public result context without forcing it.
        Projection is cached beside raw realization, independently per world."
       ([ctx value]
        (contextualize ctx value
                       (when (sequence-data? value) (-seq-project value))))
       ([ctx value project]
        (cond
         (instance? ManagedSeq value)
         (let [^ManagedSeq value value]
           (if (.-owner value)
             (->ManagedSeq (.-owner value) ctx nil (.-metadata value) project)
             (contextualize ctx (.-value value) project)))
         (instance? ManagedChunkedSeq value)
         (contextualize ctx (.-delegate ^ManagedChunkedSeq value) project)
         (and (seq? value) (nil? (fork/affine-resource-kind value)))
         (if project
           ;; A concrete seq still needs a raw logical owner: tails returned to
           ;; the host must re-project their cached heads in another world.
           (world/call-with-context
            ctx
            #(->ManagedSeq (make-owner {:phase :raw :value value})
                           ctx nil (meta value) project))
           (sequence-view ctx value))
         :else value)))

     (defn- projection-data! [owner project]
       (let [^SeqBox b (owner-box owner)]
         (with-box b
           (fn []
             (let [cached @(.-projected b)]
               (if (and cached (identical? project (:project cached)))
                 (:value cached)
                 (let [ctx (store/get-ctx)
                       raw (owner-value owner)
                       data
                       (if-let [source (.-projection-source ^Owner owner)]
                         ;; CLJS metadata aliases retain their own pending
                         ;; marker but share the source's canonical elements.
                         ;; This logical source link remains world-selecting.
                         (projection-data! source project)
                         (when raw
                          (if (chunked-seq? raw)
                           (let [c (chunk-first raw) n (count c) out (chunk-buffer n)]
                             (dotimes [i n]
                               (chunk-append out (project ctx (nth c i))))
                             (chunk-cons (chunk out)
                                         (contextualize ctx (chunk-rest raw) project)))
                           (cons (project ctx (first raw))
                                 (contextualize ctx (rest raw) project)))))]
                   ;; Keep only this node's projected head/chunk and tail.
                   ;; Fork copies raw state only; wrappers bound to this world
                   ;; never enter a child's realization cache.
                   (vreset! (.-projected b) {:project project :value data})
                   data)))))))

     (defn- seq-value [value]
       (call-seq value
         #(when-let [data (-seq-data value)]
            (if (-seq-project value)
              ;; Preserve the raw owner at host tail boundaries. Returning an
              ;; ownerless view of projected data would freeze parent wrappers
              ;; into a retained tail subsequently consumed in a child.
              (if (and (chunked-seq? data)
                       (not (instance? ManagedChunkedSeq value)))
                (->ManagedChunkedSeq value)
                value)
              (sequence-view (store/get-ctx) data)))))

     (defn- seq-first [value]
       (call-seq value #(first (-seq-data value))))

     (defn- seq-next [value]
       (call-seq value #(contextualize (store/get-ctx) (next (-seq-data value)))))

     (defn- seq-rest [value]
       (call-seq value #(contextualize (store/get-ctx) (rest (-seq-data value)))))

     (defn- reduction-data [initial]
       (loop [value initial]
         (interrupt!)
         (if (sequence-data? value)
           ;; Every managed tail may carry a selected-world projection. Raw
           ;; normalization alone would unwrap its owner without projecting.
           (recur (call-seq value #(-seq-data value)))
           (let [s (seq (checked-input value))]
             (if (sequence-data? s)
               (recur s)
               (checked-input s))))))

     (defn- reduce-chunk [f init chunk]
       (let [size (count chunk)]
         (loop [index 0 result init]
           (if (< index size)
             (do
               ;; Even an unusually large native chunk remains cancellable.
               (when (zero? (bit-and index 31)) (interrupt!))
               (let [result (f result (nth chunk index))]
                 (if (reduced? result) result (recur (inc index) result))))
             result))))

     (defn- reduce-segments [f init initial]
       (loop [result init s (reduction-data initial)]
         (if s
           (let [chunked? (chunked-seq? s)
                 result (if chunked?
                          (reduce-chunk f result (chunk-first s))
                          (f result (first s)))]
             (if (reduced? result)
               @result
               ;; Do not ask next to force a foreign tail before validation.
               (recur result (reduction-data (if chunked? (chunk-rest s) (rest s))))))
           result)))

     (defn- seq-reduce
       ([value f]
        (call-seq value
          #(if-let [s (reduction-data value)]
             (reduce-segments f (first s) (rest s))
             (f))))
       ([value f init]
        (call-seq value #(reduce-segments f init value))))
     (defn- seq-count [value] (call-seq value #(count (-seq-data value))))
     (defn- seq-meta-clone [value m]
       (call-seq value #(contextualize (store/get-ctx) (-seq-meta-clone value m) (-seq-project value))))
     (defn- seq-realized? [value] (call-seq value #(-seq-realized? value)))
     (defn- seq-string [value] (call-seq value #(pr-str (-seq-data value))))

     (defn- seq-equiv [value other equivalent?]
       (boolean
        (and (or (sequential? other) #?(:clj (instance? List other) :cljs false))
            (call-seq value
              #(loop [a (-seq-data value)
                      b (if (sequence-data? other)
                          (call-seq other (fn [] (-seq-data other)))
                          (seq other))]
                 (if a
                   (and b
                        (if equivalent? (= (first a) (first b))
                            #?(:clj (clojure.lang.Util/equals (first a) (first b))
                               :cljs (= (first a) (first b))))
                        (recur (next a) (next b)))
                   (nil? b)))))))

     (defn- seq-hash [value]
       (call-seq value #(#?(:clj Murmur3/hashOrdered :cljs hash-ordered-coll) (or (-seq-data value) (quote ())))))

     #?(:clj
        (defn- seq-java-hash [value]
          (call-seq value
            #(reduce (fn [h x] (unchecked-add-int (unchecked-multiply-int 31 h)
                                                (if (nil? x) 0 (.hashCode ^Object x))))
                     1 (-seq-data value)))))

     #?(:clj
        (defn- java-list [value] (clojure.core/lazy-seq (seq value))))
     #?(:clj
        (defn- seq-iterator [value] (SeqIterator. ^ISeq (seq value))))
     (defn- chunk-first* [value]
       (call-seq value #(chunk-first (-seq-data value))))
     (defn- chunk-next* [value]
       (call-seq value #(contextualize (store/get-ctx) (chunk-next (-seq-data value)))))
     (defn- chunk-rest* [value]
       (call-seq value #(or (contextualize (store/get-ctx) (chunk-rest (-seq-data value))) '())))))
