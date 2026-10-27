(ns sci.lazy-producers-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.lazy-producers :as producers]))

(defn- forkable-context []
  (sci/init {:runtime-mode :forkable
             :namespaces {'clojure.core producers/producer-functions}}))

(defn- evaluate [ctx code]
  (sci/eval-string* ctx code))

(deftest ordinary-producer-values-match-standard-mode
  (doseq [form ["(vec (map inc [1 2 3]))"
                "(vec (for [x [1 2] y [:a :b] :when (odd? x)] [x y]))"
                "(vec (lazy-cat [1 2] [3 4]))"
                "(vec (map + [1 2] [10 20]))"
                "(vec (map + [1 2] [10 20] [100 200]))"
                "(vec (map + [1 2] [10 20] [100 200] [1000 2000]))"
                "(vec (map-indexed vector [:a :b :c]))"
                "(vec (keep #(when (odd? %) %) [1 2 3 4]))"
                "(vec (keep-indexed #(when (odd? %1) [%1 %2]) [:a :b :c :d]))"
                "(vec (filter odd? [1 2 3 4]))"
                "(vec (remove odd? [1 2 3 4]))"
                "(vec (concat [1 2] nil [3] [4 5]))"
                "(vec (mapcat #(vector % (inc %)) [1 3]))"
                "(vec (take 3 (repeat :x)))"
                "(vec (take 5 (iterate inc 0)))"
                "(vec (take 7 (cycle [1 2 3])))"
                "(nil? (cycle []))"
                "(nil? (repeat 0 :x))"
                "(vec (repeat 2.5 :x))"
                #?(:clj "(vec (replicate 2.5 :x))")
                "(replace {1 :one} [1 2 1])"
                "(vec (replace {1 :one} (list 1 2 1)))"
                "(vec (subseq (sorted-set 1 2 3 4) < 3))"
                "(vec (subseq (sorted-set 1 2 3 4) > 2))"
                "(vec (subseq (sorted-set 1 2 3 4) >= 2 < 4))"
                "(vec (subseq (sorted-map 1 :a 2 :b 3 :c 4 :d) >= 2 < 4))"
                "(vec (subseq (sorted-set-by > 1 2 3 4) >= 3))"
                "(vec (rsubseq (sorted-set 1 2 3 4) > 2))"
                "(vec (rsubseq (sorted-set 1 2 3 4) < 3))"
                "(vec (rsubseq (sorted-set 1 2 3 4) >= 2 < 4))"
                "(vec (rsubseq (sorted-map 1 :a 2 :b 3 :c 4 :d) >= 2 < 4))"
                "(vec (dedupe []))"
                "(vec (drop 2 [1 2 3 4]))"
                "(vec (take-while #(< % 3) [1 2 3 1]))"
                "(vec (drop-while #(< % 3) [1 2 3 1]))"
                "(vec (take-nth 2 [1 2 3 4 5]))"
                "(vec (drop-last 2 [1 2 3 4]))"
                "(vec (take-last 2 [1 2 3 4]))"
                "(vec (distinct [1 2 1 3 2]))"
                "(vec (dedupe [1 1 2 2 1]))"
                "(vec (interleave [1 2] [:a :b] [:x :y]))"
                "(vec (interpose :x [1 2 3]))"
                "(mapv vec (partition 3 2 [:pad] [1 2 3 4]))"
                "(mapv vec (partition-all 3 2 [1 2 3 4]))"
                "(mapv vec (partition-by odd? [1 3 2 4 5]))"
                "(vec (reductions + [1 2 3]))"
                "(vec (reductions + 10 [1 2 3]))"
                "(vec (flatten [1 [2 [3]] nil]))"
                "(mapv vec (split-at 2 [1 2 3 4]))"
                "(mapv vec (split-with odd? [1 3 2 5]))"
                "(vec (random-sample 0 [1 2 3]))"
                "(vec (random-sample 1 [1 2 3]))"
                "(vec (range 3))"
                "(vec (range 5 -2 -2))"
                "(vec (range 0.2 1.0 0.2))"
                "(vec (take 3 (range 1 4 0)))"
                "(vec (take 4 (range)))"
                "(vec (take 5 (mapcat identity (repeat [1 2]))))"
                "(vec (take 5 (apply concat (repeat [1 2]))))"]]
    (testing form
      (is (= (evaluate (sci/init {}) form)
             (evaluate (forkable-context) form))))))

(deftest producer-caches-are-local-and-preserve-chunking
  (let [first-call-count
        (evaluate (sci/init {})
                  "(let [calls (atom 0) xs (map (fn [x] (swap! calls inc) x) [1 2 3])]
                     (first xs) @calls)")
        parent (forkable-context)]
    (evaluate parent "(def calls (atom 0))
                      (def xs (map (fn [x] (swap! calls inc) (+ x 10)) [1 2 3]))
                      (def boxed {:items xs})
                      (def get-items (fn [] (:items boxed)))")
    (let [child (sci/fork parent)]
      (is (= 0 (evaluate parent "@calls")) "forking did not evaluate the producer")
      (is (= 11 (evaluate child "(first (get-items))")))
      (is (= first-call-count (evaluate child "@calls"))
          "vector input preserves this host's chunked or unchunked realization")
      (is (= 0 (evaluate parent "@calls")) "child cache filling does not execute in its parent")
      (is (= [11 12 13] (evaluate child "(vec (get-items))")))
      (is (= 3 (evaluate child "@calls")) "already-realized output is not repeated")
      (is (= 11 (evaluate parent "(first (get-items))")))
      (is (= first-call-count (evaluate parent "@calls")))
      (let [after-realization (sci/fork parent)]
        (is (= [11 12 13] (evaluate after-realization "(vec xs)")))
        (is (= 3 (evaluate after-realization "@calls")) "fork inherits the realized prefix")))))

(deftest iterate-function-realization-is-not-shared
  (let [parent (forkable-context)]
    (evaluate parent "(def calls (atom 0))
                      (def xs (iterate (fn [x] (swap! calls inc) (inc x)) 0))")
    (let [child (sci/fork parent)]
      (is (= 0 (evaluate child "(first xs)")))
      (is (= 0 (evaluate child "@calls")))
      (is (= 2 (evaluate child "(nth xs 2)")))
      (is (= 2 (evaluate child "@calls")))
      (is (= 0 (evaluate parent "@calls")))
      (is (= 1 (evaluate parent "(nth xs 1)")))
      (is (= 1 (evaluate parent "@calls")))
      (is (= 2 (evaluate child "@calls"))))))

(deftest ordinary-transducer-results-match-standard-mode
  (doseq [form ["(into [] (map-indexed vector) [:a :b :c])"
                "(into [] (keep-indexed #(when (odd? %1) [%1 %2])) [:a :b :c :d])"
                "(into [] (comp (map inc) (filter odd?) (take 2)) [0 1 2 3 4])"
                "(into [] (drop 2) [1 2 3 4])"
                "(into [] (drop-while odd?) [1 3 2 5])"
                "(into [] (take-nth 2) [1 2 3 4 5])"
                "(into [] (distinct) [1 2 1 3 2])"
                "(into [] (dedupe) [1 1 2 2 1])"
                "(into [] (replace {1 :one}) [1 2 1])"
                "(into [] (interpose :x) [1 2 3])"
                "(into [] (partition-all 2) [1 2 3 4 5])"
                "(into [] (partition-by odd?) [1 3 2 4 5])"]]
    (testing form
      (is (= (evaluate (sci/init {}) form)
             (evaluate (forkable-context) form))))))

(deftest stateful-reducing-functions-can-fork-mid-input
  (let [parent (forkable-context)]
    (evaluate parent "(def rf ((comp (map-indexed vector) (take 3) (partition-all 2)) conj))
                      (def partial-result (rf [] :a))")
    (let [child (sci/fork parent)]
      (is (= [[[0 :a] [1 :b]]]
             (evaluate child "(let [result (rf partial-result :b)] (rf result))")))
      (is (= [[[0 :a] [1 :c]]]
             (evaluate parent "(let [result (rf partial-result :c)] (rf result))"))))))

(deftest each-stateful-transducer-can-fork-mid-input
  (doseq [xform ["(map-indexed vector)" "(keep-indexed vector)" "(take 3)"
                 "(drop 2)" "(take-nth 2)" "(drop-while odd?)" "(distinct)"
                 "(dedupe)" "(interpose :between)" "(partition-all 2)"
                 "(partition-by odd?)"]]
    (testing xform
      (let [setup (str "(def rf (" xform " conj)) (def prefix (rf [] 1))")
            finish (fn [input]
                     (str "(let [result (reduce rf prefix " input ")] (rf result))"))
            parent (forkable-context)
            _ (evaluate parent setup)
            child (sci/fork parent)
            expected (fn [input]
                       (let [ctx (sci/init {})]
                         (evaluate ctx setup)
                         (evaluate ctx (finish input))))]
        (is (= (expected "[2 3 4]") (evaluate child (finish "[2 3 4]"))))
        (is (= (expected "[1 5 6]") (evaluate parent (finish "[1 5 6]"))))))))

(deftest nested-empty-managed-results-do-not-grow-the-call-stack
  (let [ctx (forkable-context)]
    (is (= 50000 (evaluate ctx "(first (filter #(= % 50000) (range 100000)))")))
    (is (= 50000 (evaluate ctx "(first (keep #(when (= % 50000) %) (range 100000)))")))))

(deftest sequence-cursor-boundary-is-explicit
  (let [ctx (forkable-context)]
    (is (= [1 2] (evaluate ctx "(vec (sequence [1 2]))")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"managed reducing cursor"
                          (evaluate ctx "(sequence (map inc) [1 2])")))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"managed reducing cursor"
                          (evaluate ctx "(eduction (map inc) [1 2])")))))
(deftest partition-segment-realization-matches-its-host
  (let [form "(let [calls (atom 0)
                    source (map (fn [x] (swap! calls inc) x) (list 1 2 3 4 5))
                    parts (partition-all 2 source)]
                (first parts)
                @calls)"]
    (is (= (evaluate (sci/init {}) form)
           (evaluate (forkable-context) form)))))

(deftest dedupe-preserves-chunk-lookahead
  (let [form "(let [calls (atom 0)
                    source (map (fn [x] (swap! calls inc) x) (range 100))
                    result (dedupe source)]
                (let [at-construction @calls]
                  (first result)
                  [at-construction @calls]))"]
    (is (= (evaluate (sci/init {}) form)
           (evaluate (forkable-context) form)))))
