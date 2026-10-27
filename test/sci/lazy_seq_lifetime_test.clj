(ns sci.lazy-seq-lifetime-test
  "Reachability regressions for managed lazy values, independent of heap size."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.lazy-seq]
            [sci.impl.world :as world])
  (:import [java.lang.ref ReferenceQueue WeakReference]))

(defn- tracked-reference [^ReferenceQueue queue value]
  (WeakReference. value queue))

(defn- reachable-count [references]
  (reduce (fn [n ^WeakReference reference]
            (if (nil? (.get reference)) n (inc n)))
          0 references))

(defn- collect-unreachable!
  "Request GC until all references clear or a bounded deadline expires.
   The queue provides a bounded wait rather than assuming one immediate GC."
  [^ReferenceQueue queue references]
  (let [deadline (+ (System/nanoTime) 10000000000)]
    (loop [attempt 0]
      (cond
        (zero? (reachable-count references)) true
        (or (= attempt 80) (>= (System/nanoTime) deadline)) false
        :else
        (do
          (System/gc)
          (.remove queue 20)
          (recur (inc attempt)))))))

(defn- lazy-owner [^sci.impl.lazy_seq.ManagedSeq value]
  (.-owner value))

(defn- tracker [^ReferenceQueue queue seqs owners]
  (fn [value]
    (swap! seqs conj (tracked-reference queue value))
    (swap! owners conj (tracked-reference queue (lazy-owner value)))
    value))

(defn- consume-anonymous-prefix!
  "Keep strong evaluation locals off the stack while the caller requests GC."
  [ctx n]
  (sci/eval-string*
   ctx
   (str "(defn temporary-cells [n]
           (track-seq!
             (lazy-seq
               (when (< n " n ")
                 (cons (new-payload!) (temporary-cells (inc n)))))))
         (dorun (temporary-cells 0))")))

(deftest consumed-prefixes-and-payloads-collect-without-forking
  (let [queue (ReferenceQueue.)
        seqs (atom [])
        owners (atom [])
        payloads (atom [])
        ctx (sci/init
             {:runtime-mode :forkable
              :bindings
              {'track-seq! (tracker queue seqs owners)
               'new-payload! (fn []
                               (let [payload (Object.)]
                                 (swap! payloads conj (tracked-reference queue payload))
                                 payload))}})]
    (try
      (consume-anonymous-prefix! ctx 4000)
      (is (= 4000 (count @payloads)))
      (is (= 4001 (count @owners)))
      (let [references (into [] cat [@seqs @owners @payloads])]
        (is (collect-unreachable! queue references)
            (str "consumed values must not remain rooted by a live world: "
                 {:sequences (reachable-count @seqs)
                  :owners (reachable-count @owners)
                  :payloads (reachable-count @payloads)})))
      (is (= :still-open (sci/eval-string* ctx ":still-open"))
          "collection occurs while the interpreter remains live, without a fork sweep")
      (finally (sci/retire! ctx)))))

(defn- realize-and-drop-cycle! [ctx]
  (sci/eval-string*
   ctx
   "(def temporary-cycle
      (track-seq!
        (lazy-seq (cons (new-payload!) temporary-cycle))))
    (first temporary-cycle)
    (def temporary-cycle nil)
    nil"))

(deftest an-unreachable-realized-self-cycle-does-not-root-its-owner
  (let [queue (ReferenceQueue.)
        seqs (atom [])
        owners (atom [])
        payloads (atom [])
        ctx (sci/init
             {:runtime-mode :forkable
              :bindings
              {'track-seq! (tracker queue seqs owners)
               'new-payload! (fn []
                               (let [payload (Object.)]
                                 (swap! payloads conj (tracked-reference queue payload))
                                 payload))}})]
    (try
      (realize-and-drop-cycle! ctx)
      (is (= 1 (count @owners)))
      (is (= 1 (count @payloads)))
      (is (collect-unreachable! queue (into [] cat [@seqs @owners @payloads]))
          "weak owner indexing must collect a realized node whose tail points back to itself")
      (is (= :still-open (sci/eval-string* ctx ":still-open")))
      (finally (sci/retire! ctx)))))

(defn- make-metadata-alias! [ctx]
  (sci/eval-string*
   ctx
   "(def counter (atom 0))
    (defn alias-cells [n]
      (lazy-seq
        (when (< n 8)
          (swap! counter inc)
          (cons n (alias-cells (inc n))))))
    (def original (mark-original! (alias-cells 0)))
    (def alias (with-meta original {:retained :alias}))
    (def original nil)
    alias"))

(deftest a-reachable-metadata-alias-outlives-the-original-wrapper
  (let [queue (ReferenceQueue.)
        original (atom nil)
        ctx (sci/init
             {:runtime-mode :forkable
              :bindings {'mark-original! (fn [value]
                                           (reset! original (tracked-reference queue value))
                                           value)}})]
    (try
      (let [alias (make-metadata-alias! ctx)]
        (is (collect-unreachable! queue [@original])
            "metadata aliases must not need the original sequence wrapper to stay live")
        (is (= {:retained :alias} (meta alias)))
        (is (= [0 1 2 3] (sci/call-with-context ctx #(vec (take 4 alias)))))
        (is (= 4 (sci/eval-string* ctx "@counter")))
        (let [child (sci/fork ctx)]
          (try
            (is (= [4 101]
                   (sci/eval-string* child "(reset! counter 100) [(nth alias 4) @counter]")))
            (is (= 4 (sci/eval-string* ctx "@counter")))
            (is (= [4 5] (sci/eval-string* ctx "[(nth alias 4) @counter]")))
            (finally (sci/retire! child)))))
      (finally (sci/retire! ctx)))))

(defn- realize-and-retire-child!
  "A child payload deliberately points back to its world. Explicit retirement
   must break the parent owner's WeakHashMap-key/value retention cycle."
  [parent ^ReferenceQueue queue children worlds]
  (let [child (sci/fork parent)]
    (swap! children conj (tracked-reference queue child))
    (swap! worlds conj (tracked-reference queue (:sci.impl/world child)))
    (try
      ;; Discard the result inside evaluation so no host result view pins the
      ;; child context after this helper returns.
      (sci/eval-string* child "(dorun (take 32 retirement-cells)) nil")
      (finally (sci/retire! child)))))

(deftest parent-held-sequences-release-retired-child-worlds-and-payloads
  (let [queue (ReferenceQueue.)
        children (atom [])
        worlds (atom [])
        seqs (atom [])
        owners (atom [])
        payloads (atom [])
        parent (sci/init
                {:runtime-mode :forkable
                 :bindings
                 {'track-seq! (tracker queue seqs owners)
                  'new-child-payload!
                  (fn []
                    (let [payload (object-array
                                   [(world/selected-world)
                                    (byte-array 16384)])]
                      (swap! payloads conj (tracked-reference queue payload))
                      payload))}})]
    (try
      (sci/eval-string*
       parent
       "(defn child-cells []
          (track-seq!
            (lazy-seq (cons (new-child-payload!) (child-cells)))))
        (def retirement-cells (child-cells))
        nil")
      ;; Keep the original owner strongly reachable from the parent's Var
      ;; while repeatedly materializing child-only continuations and payloads.
      (dotimes [_ 12]
        (realize-and-retire-child! parent queue children worlds))
      (is (= 384 (count @payloads)))
      (let [references (into [] cat [@children @worlds @payloads
                                     (subvec @seqs 1) (subvec @owners 1)])]
        (is (collect-unreachable! queue references)
            (str "retirement must release child cache cycles: "
                 {:contexts (reachable-count @children)
                  :worlds (reachable-count @worlds)
                  :payloads (reachable-count @payloads)})))
      (is (= 1 (reachable-count @owners)) "only the pending parent sequence owner remains")
      (is (false? (sci/eval-string* parent "(realized? retirement-cells)")))
      (is (= :parent-open (sci/eval-string* parent ":parent-open")))
      (finally (sci/retire! parent)))))


(defn- project-and-retire-child!
  [parent ^ReferenceQueue queue children worlds views functions child-defined?]
  (let [child (sci/fork parent)]
    (swap! children conj (tracked-reference queue child))
    (swap! worlds conj (tracked-reference queue (:sci.impl/world child)))
    (try
      (sci/eval-string* child "(def projection-label :child)")
      (when child-defined?
        ;; The registry's original function, as well as its public wrapper,
        ;; can itself retain the child context. Stale weak-key metadata must
        ;; not keep that original function alive after retirement.
        (sci/eval-string* child "(defn projection-observe [] projection-label) nil"))
      (let [returned (sci/eval-string* child "projection-cells")
            projected (first returned)]
        (is (= :child (projected)))
        (swap! views conj (tracked-reference queue returned))
        (swap! functions conj (tracked-reference queue projected)))
      (finally (sci/retire! child)))))

(deftest parent-held-sequences-release-retired-child-function-projections
  (let [queue (ReferenceQueue.)
        children (atom [])
        worlds (atom [])
        views (atom [])
        functions (atom [])
        parent (sci/init {:runtime-mode :forkable})]
    (try
      (sci/eval-string*
       parent
       "(def projection-label :parent)
        (defn projection-observe [] projection-label)
        (def projection-cells (lazy-seq (list projection-observe)))
        nil")
      ;; Projected callbacks capture the child context. Keeping their raw
      ;; source owner in the parent must not retain retired projection caches.
      (doseq [child-defined? [false true]]
        (testing (if child-defined? "child-created originals" "parent-created originals")
          (dotimes [_ 6]
            (project-and-retire-child! parent queue children worlds views functions child-defined?))))
      (is (= 12 (count @functions)))
      (is (collect-unreachable! queue (into [] cat [@children @worlds @views @functions]))
          (str "retirement must release context-bound projection cycles: "
               {:contexts (reachable-count @children)
                :worlds (reachable-count @worlds)
                :views (reachable-count @views)
                :functions (reachable-count @functions)}))
      (is (= :parent ((first (sci/eval-string* parent "projection-cells")))))
      (finally (sci/retire! parent)))))
