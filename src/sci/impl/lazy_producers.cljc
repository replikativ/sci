;; Portions adapted from clojure.core, Clojure 1.11.1:
;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sci.impl.lazy-producers
  "Forkable-mode core producers. Deferred collection work uses managed lazy
   nodes rather than host LazySeq caches. Stateful transducer arities use
   world-selecting SCI reference operations. Host cooperation is still required
   for foreign values captured inside opaque host closures/containers."
  {:no-doc true}
  (:refer-clojure :exclude [lazy-seq map map-indexed keep keep-indexed filter
                            remove concat mapcat take drop take-while drop-while
                            take-nth drop-last take-last distinct interleave
                            interpose partition partition-all partition-by
                            reductions repeatedly tree-seq flatten split-at
                            split-with random-sample repeat iterate cycle range
                            dedupe sequence eduction replace replicate subseq rsubseq])
  (:require [sci.impl.core-protocols :as managed]
            [sci.impl.lazy-seq :as lazyseq]
            #?@(:cljd []
                :clj [[sci.impl.lazy-seq-macros :refer [lazy-seq]]]))
  #?(:cljs (:require-macros [sci.impl.lazy-seq-macros :refer [lazy-seq]])))

#?(:cljd
   (do
     ;; Keep standard Dart loading usable. Managed lazy realization currently
     ;; requires the JVM or JavaScript weak-owner implementation.
     (def producer-functions {})
     (defn- unsupported-runtime []
       (throw (ex-info "Managed lazy realization requires JVM or JavaScript"
                       {:type ::unsupported-runtime})))
     (defn range [& _args] (unsupported-runtime))
     (defn repeat [& _args] (unsupported-runtime))
     (defn cycle [& _args] (unsupported-runtime))
     (defn iterate [& _args] (unsupported-runtime)))
   :default
   (do

     (def ^:private none #?(:clj :clojure.core/none :cljs :cljs.core/none :cljd :cljd.core/none))

     (declare map map-indexed keep keep-indexed filter remove concat mapcat take drop
              take-while drop-while take-nth drop-last take-last distinct interleave
              interpose partition partition-all partition-by reductions repeatedly
              tree-seq flatten split-at split-with random-sample repeat iterate cycle
              range dedupe sequence eduction)

     (defn map
       ([f]
        (fn [rf]
          (fn ([] (rf))
            ([result] (rf result))
            ([result input] (rf result (f input)))
            ([result input & inputs] (rf result (apply f input inputs))))))
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (if (chunked-seq? s)
               (let [c (chunk-first s) size (int (count c)) b (chunk-buffer size)]
                 (dotimes [i size] (chunk-append b (f (nth c i))))
                 (chunk-cons (chunk b) (map f (chunk-rest s))))
               (cons (f (first s)) (map f (rest s))))))))
       ([f c1 c2]
        (let [c1 (lazyseq/checked-input c1) c2 (lazyseq/checked-input c2)]
          (lazy-seq
           (let [s1 (seq c1) s2 (seq c2)]
             (when (and s1 s2) (cons (f (first s1) (first s2)) (map f (rest s1) (rest s2))))))))
       ([f c1 c2 c3]
        (let [c1 (lazyseq/checked-input c1)
              c2 (lazyseq/checked-input c2)
              c3 (lazyseq/checked-input c3)]
          (lazy-seq
           (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
             (when (and s1 s2 s3)
               (cons (f (first s1) (first s2) (first s3)) (map f (rest s1) (rest s2) (rest s3))))))))
       ([f c1 c2 c3 & colls]
        (let [c1 (lazyseq/checked-input c1)
              c2 (lazyseq/checked-input c2)
              c3 (lazyseq/checked-input c3)
              colls (map lazyseq/checked-input (lazyseq/checked-input colls))]
          (let [step (fn step [cs]
                       (lazy-seq
                        (let [ss (map seq cs)]
                          (when (every? identity ss) (cons (map first ss) (step (map rest ss)))))))]
            (map #(apply f %) (step (conj colls c3 c2 c1)))))))
     (defn map-indexed
       ([f]
        (fn [rf]
          (let [i (managed/forkable-volatile!* -1)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input] (rf result (f (managed/forkable-vswap!* i inc) input)))))))
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (letfn
           [(mapi
              [idx coll]
              (lazy-seq
               (when-let [s (seq coll)]
                 (if (chunked-seq? s)
                   (let [c (chunk-first s) size (int (count c)) b (chunk-buffer size)]
                     (dotimes [i size] (chunk-append b (f (+ idx i) (nth c i))))
                     (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                   (cons (f idx (first s)) (mapi (inc idx) (rest s)))))))]
            (mapi 0 coll)))))
     (defn keep
       ([f]
        (fn [rf]
          (fn ([] (rf))
            ([result] (rf result))
            ([result input] (let [v (f input)] (if (nil? v) result (rf result v)))))))
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (if (chunked-seq? s)
               (let [c (chunk-first s) size (count c) b (chunk-buffer size)]
                 (dotimes [i size] (let [x (f (nth c i))] (when-not (nil? x) (chunk-append b x))))
                 (chunk-cons (chunk b) (keep f (chunk-rest s))))
               (let [x (f (first s))] (if (nil? x) (keep f (rest s)) (cons x (keep f (rest s)))))))))))
     (defn keep-indexed
       ([f]
        (fn [rf]
          (let [iv (managed/forkable-volatile!* -1)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [i (managed/forkable-vswap!* iv inc) v (f i input)]
                 (if (nil? v) result (rf result v))))))))
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (letfn
           [(keepi
              [idx coll]
              (lazy-seq
               (when-let [s (seq coll)]
                 (if (chunked-seq? s)
                   (let [c (chunk-first s) size (count c) b (chunk-buffer size)]
                     (dotimes [i size]
                       (let [x (f (+ idx i) (nth c i))] (when-not (nil? x) (chunk-append b x))))
                     (chunk-cons (chunk b) (keepi (+ idx size) (chunk-rest s))))
                   (let [x (f idx (first s))]
                     (if (nil? x)
                       (keepi (inc idx) (rest s))
                       (cons x (keepi (inc idx) (rest s)))))))))]
            (keepi 0 coll)))))
     (defn filter
       ([pred]
        (fn [rf]
          (fn ([] (rf))
            ([result] (rf result))
            ([result input] (if (pred input) (rf result input) result)))))
       ([pred coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (if (chunked-seq? s)
               (let [c (chunk-first s) size (count c) b (chunk-buffer size)]
                 (dotimes [i size] (let [v (nth c i)] (when (pred v) (chunk-append b v))))
                 (chunk-cons (chunk b) (filter pred (chunk-rest s))))
               (let [f (first s) r (rest s)]
                 (if (pred f) (cons f (filter pred r)) (filter pred r)))))))))
     (defn remove
       ([pred] (filter (complement pred)))
       ([pred coll] (let [coll (lazyseq/checked-input coll)] (filter (complement pred) coll))))
     (defn concat
       ([] (lazy-seq nil))
       ([x] (let [x (lazyseq/checked-input x)] (lazy-seq x)))
       ([x y]
        (let [x (lazyseq/checked-input x) y (lazyseq/checked-input y)]
          (lazy-seq
           (let [s (seq x)]
             (if s
               (if (chunked-seq? s)
                 (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
                 (cons (first s) (concat (rest s) y)))
               y)))))
       ([x y & zs]
        (let [x (lazyseq/checked-input x)
              y (lazyseq/checked-input y)
              zs (map lazyseq/checked-input (lazyseq/checked-input zs))]
          (let [cat (fn cat [xys zs]
                      (lazy-seq
                       (let [xys (seq xys)]
                         (if xys
                           (if (chunked-seq? xys)
                             (chunk-cons (chunk-first xys) (cat (chunk-rest xys) zs))
                             (cons (first xys) (cat (rest xys) zs)))
                           (when zs (cat (first zs) (next zs)))))))]
            (cat (concat x y) zs)))))
     (defn mapcat
       ([f] (comp (map f) cat))
       ([f & colls]
        (let [colls (map lazyseq/checked-input (lazyseq/checked-input colls))]
          (apply concat (apply map f colls)))))
     (defn take
       ([n]
        (fn [rf]
          (let [nv (managed/forkable-volatile!* n)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [n (managed/forkable-deref* nv)
                     nn (managed/forkable-vswap!* nv dec)
                     result (if (pos? n) (rf result input) result)]
                 (if (not (pos? nn)) (ensure-reduced result) result)))))))
       ([n coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when (pos? n) (when-let [s (seq coll)] (cons (first s) (take (dec n) (rest s)))))))))
     (defn drop
       ([n]
        (fn [rf]
          (let [nv (managed/forkable-volatile!* n)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [n (managed/forkable-deref* nv)]
                 (managed/forkable-vswap!* nv dec)
                 (if (pos? n) result (rf result input))))))))
       ([n coll]
        (let [coll (lazyseq/checked-input coll)]
          (let [step (fn [n coll]
                       (let [s (seq coll)] (if (and (pos? n) s) (recur (dec n) (rest s)) s)))]
            (lazy-seq (step n coll))))))
     (defn take-while
       ([pred]
        (fn [rf]
          (fn ([] (rf))
            ([result] (rf result))
            ([result input] (if (pred input) (rf result input) (reduced result))))))
       ([pred coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (when (pred (first s)) (cons (first s) (take-while pred (rest s)))))))))
     (defn drop-while
       ([pred]
        (fn [rf]
          (let [dv (managed/forkable-volatile!* true)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [drop? (managed/forkable-deref* dv)]
                 (if (and drop? (pred input))
                   result
                   (do (managed/forkable-vreset!* dv nil) (rf result input)))))))))
       ([pred coll]
        (let [coll (lazyseq/checked-input coll)]
          (let [step (fn [pred coll]
                       (let [s (seq coll)] (if (and s (pred (first s))) (recur pred (rest s)) s)))]
            (lazy-seq (step pred coll))))))
     (defn take-nth
       ([n]
        (fn [rf]
          (let [iv (managed/forkable-volatile!* -1)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [i (managed/forkable-vswap!* iv inc)]
                 (if (zero? (rem i n)) (rf result input) result)))))))
       ([n coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq (when-let [s (seq coll)] (cons (first s) (take-nth n (drop n s))))))))
     (defn drop-last ([coll] (drop-last 1 coll)) ([n coll] (map (fn [x _] x) coll (drop n coll))))
     (defn take-last [n coll]
       (let [coll (lazyseq/checked-input coll)]
         (loop [s (seq coll) lead (seq (drop n coll))] (if lead (recur (next s) (next lead)) s))))
     (defn distinct
       ([]
        (fn [rf]
          (let [seen (managed/forkable-volatile!* #{})]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (if (contains? (managed/forkable-deref* seen) input)
                 result
                 (do (managed/forkable-vswap!* seen conj input) (rf result input))))))))
       ([coll]
        (let [coll (lazyseq/checked-input coll)]
          (let [step (fn step [xs seen]
                       (lazy-seq
                        ((fn [[f :as xs] seen]
                           (when-let [s (seq xs)]
                             (if (contains? seen f)
                               (recur (rest s) seen)
                               (cons f (step (rest s) (conj seen f))))))
                         xs
                         seen)))]
            (step coll #{})))))
     (defn interleave
       ([] ())
       ([c1] (let [c1 (lazyseq/checked-input c1)] (lazy-seq c1)))
       ([c1 c2]
        (let [c1 (lazyseq/checked-input c1) c2 (lazyseq/checked-input c2)]
          (lazy-seq
           (let [s1 (seq c1) s2 (seq c2)]
             (when (and s1 s2)
               (cons (first s1) (cons (first s2) (interleave (rest s1) (rest s2)))))))))
       ([c1 c2 & colls]
        (let [c1 (lazyseq/checked-input c1)
              c2 (lazyseq/checked-input c2)
              colls (map lazyseq/checked-input (lazyseq/checked-input colls))]
          (lazy-seq
           (let [ss (map seq (conj colls c2 c1))]
             (when (every? identity ss) (concat (map first ss) (apply interleave (map rest ss)))))))))
     (defn interpose
       ([sep]
        (fn [rf]
          (let [started (managed/forkable-volatile!* false)]
            (fn ([] (rf))
              ([result] (rf result))
              ([result input]
               (if (managed/forkable-deref* started)
                 (let [sepr (rf result sep)] (if (reduced? sepr) sepr (rf sepr input)))
                 (do (managed/forkable-vreset!* started true) (rf result input))))))))
       ([sep coll] (let [coll (lazyseq/checked-input coll)] (drop 1 (interleave (repeat sep) coll)))))
     (defn partition
       ([n coll] (let [coll (lazyseq/checked-input coll)] (partition n n coll)))
       ([n step coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (let [p (doall (take n s))]
               (when (= n (count p))
                 (cons p (partition n step #?(:cljs (drop step s) :default (nthrest s step))))))))))
       ([n step pad coll]
        (let [pad (lazyseq/checked-input pad) coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (let [p (doall (take n s))]
               (if (= n (count p))
                 (cons p (partition n step pad #?(:cljs (drop step s) :default (nthrest s step))))
                 (list (take n (concat p pad))))))))))
     (defn reductions
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq (if-let [s (seq coll)] (reductions f (first s) (rest s)) (list (f))))))
       ([f init coll]
        (let [coll (lazyseq/checked-input coll)]
          (if (reduced? init)
            (list (managed/forkable-deref* init))
            (cons
             init
             (lazy-seq (when-let [s (seq coll)] (reductions f (f init (first s)) (rest s)))))))))
     (defn repeatedly ([f] (lazy-seq (cons (f) (repeatedly f)))) ([n f] (take n (repeatedly f))))
     (defn tree-seq [branch? children root]
       (let [root (lazyseq/checked-input root)]
         (let [walk (fn walk [node]
                      (lazy-seq (cons node (when (branch? node) (mapcat walk (children node))))))]
           (walk root))))
     (defn flatten [x]
       (let [x (lazyseq/checked-input x)]
         (filter (complement sequential?) (rest (tree-seq sequential? seq x)))))
     (defn split-at [n coll] (let [coll (lazyseq/checked-input coll)] [(take n coll) (drop n coll)]))
     (defn split-with [pred coll]
       (let [coll (lazyseq/checked-input coll)] [(take-while pred coll) (drop-while pred coll)]))
     (defn random-sample
       ([prob] (filter (fn [_] (< (rand) prob))))
       ([prob coll] (let [coll (lazyseq/checked-input coll)] (filter (fn [_] (< (rand) prob)) coll))))

     (defn partition-all
       ([n]
        (fn [rf]
     ;; A persistent vector in a managed cell replaces core's opaque ArrayList.
          (let [n #?(:clj (long n) :default n)]
            #?(:clj (let [capacity (int n)]
                      (when (neg? capacity)
                        (throw (IllegalArgumentException. (str "Illegal Capacity: " capacity))))))
            (let [buffer (managed/forkable-volatile!* [])]
              (fn
                ([] (rf))
                ([result]
                 (let [pending (managed/forkable-deref* buffer)
                       result (if (empty? pending)
                                result
                                (do
                                  (managed/forkable-vreset!* buffer [])
                                  (unreduced (rf result pending))))]
                   (rf result)))
                ([result input]
                 (let [items (managed/forkable-vswap!* buffer conj input)]
                   (if (= n (count items))
                     (do (managed/forkable-vreset!* buffer []) (rf result items))
                     result))))))))
       ([n coll] (partition-all n n coll))
       ([n step coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             #?(:cljs (cons (take n s) (partition-all n step (drop step s)))
                :default (let [part (doall (take n s))]
                           (cons part (partition-all n step (nthrest s step))))))))))

     (defn partition-by
       ([f]
        (fn [rf]
          (let [buffer (managed/forkable-volatile!* [])
                previous (managed/forkable-volatile!* none)]
            (fn
              ([] (rf))
              ([result]
               (let [pending (managed/forkable-deref* buffer)
                     result (if (empty? pending)
                              result
                              (do
                                (managed/forkable-vreset!* buffer [])
                                (unreduced (rf result pending))))]
                 (rf result)))
              ([result input]
               (let [prior (managed/forkable-deref* previous)
                     value (f input)]
                 (managed/forkable-vreset!* previous value)
                 (if (or #?(:cljs (= prior none) :default (identical? prior none)) (= value prior))
                   (do (managed/forkable-vswap!* buffer conj input) result)
                   (let [pending (managed/forkable-deref* buffer)]
                     (managed/forkable-vreset!* buffer [])
                     (let [ret (rf result pending)]
                       (when-not (reduced? ret)
                         (managed/forkable-vreset!* buffer [input]))
                       ret)))))))))
       ([f coll]
        (let [coll (lazyseq/checked-input coll)]
          (lazy-seq
           (when-let [s (seq coll)]
             (let [fst (first s)
                   value (f fst)
                   part (cons fst (take-while #(= value (f %)) (next s)))]
               (cons part (partition-by f (lazy-seq (drop (count part) s))))))))))

     (defn- next-dedupe-value [coll previous]
       (loop [input coll]
         (when-let [s (seq input)]
           (let [value (first s)]
             (if (= value previous)
               (recur (rest s))
               [value (rest s)])))))

     (defn- dedupe-step [coll previous]
  ;; Native sequence first asks its iterator for a pending value before making
  ;; the lazy chunk. Preserve that single-output lookahead without retaining
  ;; a host TransformerIterator or a mutable reducing cursor.
       (when-let [[ready input] (next-dedupe-value coll previous)]
         (lazy-seq
          (let [buffer (chunk-buffer 32)]
            (chunk-append buffer ready)
            (loop [input input previous ready emitted 1]
              (if (= emitted 32)
                (chunk-cons (chunk buffer) (dedupe-step input previous))
                (if-let [[value remaining] (next-dedupe-value input previous)]
                  (do (chunk-append buffer value)
                      (recur remaining value (inc emitted)))
                  (chunk-cons (chunk buffer) nil))))))))

     (defn dedupe
       ([]
        (fn [rf]
          (let [previous (managed/forkable-volatile!* none)]
            (fn
              ([] (rf))
              ([result] (rf result))
              ([result input]
               (let [prior (managed/forkable-deref* previous)]
                 (managed/forkable-vreset!* previous input)
                 (if (= prior input) result (rf result input))))))))
       ([coll]
        (let [coll (lazyseq/checked-input coll)]
     ;; JVM's chunk iterator prefetches before returning its LazySeq; CLJS's
     ;; chunkIteratorSeq defers even that first hasNext call.
          #?(:cljs (lazy-seq (dedupe-step coll none))
             :default (or (dedupe-step coll none) ())))))

     (defn repeat
       ([x] (lazy-seq (cons x (repeat x))))
       ([n x]
        (let [n #?(:clj (long n) :default n)]
          (lazy-seq (when (pos? n) (cons x (repeat (dec n) x)))))))

     (defn iterate [f x]
  ;; Do not invoke f while constructing the first element.
       (lazy-seq (cons x (lazy-seq (iterate f (f x))))))

     (defn cycle [coll]
  ;; Native cycle obtains its first source seq at construction too.
       (if-let [s (seq (lazyseq/checked-input coll))]
         (letfn [(step [current]
                   (lazy-seq
                    (let [current (or (seq current) s)]
                      (cons (first current) (step (rest current))))))]
           (step s))
         ()))

     (defn replicate [n x]
       (take n (repeat x)))

     (defn replace
       ([smap]
        (let [smap (lazyseq/checked-input smap)]
          (map #(if-let [entry (find smap %)] (val entry) %))))
       ([smap coll]
        (let [smap (lazyseq/checked-input smap)
              coll (lazyseq/checked-input coll)]
          (if (vector? coll)
       ;; The native vector path is entirely eager and returns a vector.
            (clojure.core/replace smap coll)
            (map #(if-let [entry (find smap %)] (val entry) %) coll)))))

     (defn- sorted-sequence [coll ascending?]
       #?(:clj (.seq ^clojure.lang.Sorted coll ascending?)
          :cljs (if (instance? cljs.core/PersistentTreeSet coll)
             ;; PersistentTreeSet implements sorted seqs using native lazy
             ;; map(key). Rebuild that pure adapter with managed map instead
             ;; of weakening the foreign LazySeq input guard.
                  (map key (-sorted-seq (.-tree-map coll) ascending?))
                  (-sorted-seq coll ascending?))
          :cljd (throw (ex-info "Managed sorted sequences require JVM or JavaScript"
                                {:type ::unsupported-runtime}))))

     (defn- sorted-sequence-from [coll key ascending?]
       #?(:clj (.seqFrom ^clojure.lang.Sorted coll key ascending?)
          :cljs (if (instance? cljs.core/PersistentTreeSet coll)
                  (map clojure.core/key (-sorted-seq-from (.-tree-map coll) key ascending?))
                  (-sorted-seq-from coll key ascending?))
          :cljd (throw (ex-info "Managed sorted sequences require JVM or JavaScript"
                                {:type ::unsupported-runtime}))))

     (defn- sorted-bound-fn [coll test key]
       (fn [entry]
         (test #?(:clj (.compare ^java.util.Comparator (.comparator ^clojure.lang.Sorted coll)
                                 (.entryKey ^clojure.lang.Sorted coll entry) key)
                  :cljs ((-comparator coll) (-entry-key coll entry) key)
                  :cljd (throw (ex-info "Managed sorted sequences require JVM or JavaScript"
                                        {:type ::unsupported-runtime})))
               0)))

     (defn subseq
       ([coll test key]
        (let [coll (lazyseq/checked-input coll)
              include (sorted-bound-fn coll test key)]
          (if (#{> >=} test)
            (when-let [[entry :as s] (sorted-sequence-from coll key true)]
              (if (include entry) s (next s)))
            (take-while include (sorted-sequence coll true)))))
       ([coll start-test start-key end-test end-key]
        (let [coll (lazyseq/checked-input coll)]
          (when-let [[entry :as s] (sorted-sequence-from coll start-key true)]
            (take-while (sorted-bound-fn coll end-test end-key)
                        (if ((sorted-bound-fn coll start-test start-key) entry) s (next s)))))))

     (defn rsubseq
       ([coll test key]
        (let [coll (lazyseq/checked-input coll)
              include (sorted-bound-fn coll test key)]
          (if (#{< <=} test)
            (when-let [[entry :as s] (sorted-sequence-from coll key false)]
              (if (include entry) s (next s)))
            (take-while include (sorted-sequence coll false)))))
       ([coll start-test start-key end-test end-key]
        (let [coll (lazyseq/checked-input coll)]
          (when-let [[entry :as s] (sorted-sequence-from coll end-key false)]
            (take-while (sorted-bound-fn coll start-test start-key)
                        (if ((sorted-bound-fn coll end-test end-key) entry) s (next s)))))))

     (defn- arithmetic-seq [source]
  ;; Only range calls this adapter. Native range's arithmetic seed/chunk caches
  ;; cannot invoke interpreted callbacks or consume external resources. They
  ;; are deliberately trusted; every exposed deferred tail is managed.
       (lazy-seq
        (when-let [s (seq source)]
          (if (chunked-seq? s)
            (chunk-cons (chunk-first s) (arithmetic-seq (chunk-rest s)))
            (cons (first s) (arithmetic-seq (rest s)))))))

     (defn range
       ([] (arithmetic-seq (clojure.core/range)))
       ([end] (arithmetic-seq (clojure.core/range end)))
       ([start end] (arithmetic-seq (clojure.core/range start end)))
       ([start end step] (arithmetic-seq (clojure.core/range start end step))))

     (defn sequence
       ([coll]
        (let [coll (lazyseq/checked-input coll)]
          (if (seq? coll) coll (or (seq coll) ()))))
       ([_xform _coll]
        (throw (ex-info "Transducer sequence requires a managed reducing cursor; use into/transduce for eager consumption"
                        {:type ::unmanaged-sequence-cursor})))
       ([_xform _coll & _colls]
        (throw (ex-info "Transducer sequence requires a managed reducing cursor; use into/transduce for eager consumption"
                        {:type ::unmanaged-sequence-cursor}))))

     (defn eduction [& _xforms]
       (throw (ex-info "Eduction requires a managed reducing cursor in forkable mode; use into/transduce for eager consumption"
                       {:type ::unmanaged-sequence-cursor})))

     (def producer-functions
       "Function values for forkable-core-overrides; standard mode retains core."
       {'map map 'map-indexed map-indexed 'keep keep 'keep-indexed keep-indexed
        'filter filter 'remove remove 'concat concat 'mapcat mapcat 'take take
        'drop drop 'take-while take-while 'drop-while drop-while 'take-nth take-nth
        'drop-last drop-last 'take-last take-last 'distinct distinct 'dedupe dedupe
        'interleave interleave 'interpose interpose 'partition partition
        'partition-all partition-all 'partition-by partition-by 'reductions reductions
        'repeatedly repeatedly 'repeat repeat 'iterate iterate 'cycle cycle 'range range
        'tree-seq tree-seq 'flatten flatten 'split-at split-at 'split-with split-with
        'random-sample random-sample 'sequence sequence 'eduction eduction
        #?@(:clj ['replicate replicate] :default [])
        'replace replace 'subseq subseq 'rsubseq rsubseq})))
