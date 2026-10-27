(ns sci.lazy-seq-test
  "The observable contract of interpreter-owned, fork-local lazy realization."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]))

(defn- forkable []
  (sci/init {:runtime-mode :forkable}))

(defn- run [ctx code]
  (sci/eval-string* ctx code))

(defn- caught [code]
  (str "(try " code " (catch "
       #?(:cljd "Object" :clj "Exception" :cljs ":default")
       " _ :failed))"))

(deftest pending-realization-follows-the-consuming-world
  (let [parent (forkable)]
    (run parent "(def counter (atom 0))
                 (def label :parent)
                 (def xs (lazy-seq (list [label (swap! counter inc)])))
                 nil")
    (let [child (sci/fork parent)]
      (testing "constructing and forking the sequence perform no work"
        (is (= [false 0] (run parent "[(realized? xs) @counter]")))
        (is (= [false 0] (run child "[(realized? xs) @counter]"))))
      (testing "child realization uses child roots and a child cache"
        (is (= [[:child 11] [:child 11] true 11]
               (run child "(def label :child)
                           (reset! counter 10)
                           [(first xs) (first xs) (realized? xs) @counter]")))
        (is (= [false 0 :parent]
               (run parent "[(realized? xs) @counter label]")))
        (is (= [[:parent 1] 1]
               (run parent "[(first xs) @counter]")))
        (is (= [[:child 11] 11]
               (run child "[(first xs) @counter]")))))))

(deftest a-realized-prefix-is-inherited-with-independent-tails
  (let [parent (forkable)]
    (run parent "(def events (atom []))
                 (def label :parent)
                 (defn cells [n]
                   (lazy-seq
                     (when (< n 3)
                       (swap! events conj [label n])
                       (cons [label n] (cells (inc n))))))
                 (def xs (cells 0))
                 (first xs)")
    (let [child (sci/fork parent)]
      (is (= [[:parent 0]] (run child "@events")) "fork does not force the tail")
      (is (= [[[:parent 0] [:child 1] [:child 2]]
              [[:parent 0] [:child 1] [:child 2]]]
             (run child "(def label :child) [(vec xs) @events]")))
      (is (= [[:parent 0]] (run parent "@events")))
      (is (= [[[:parent 0] [:parent 1] [:parent 2]]
              [[:parent 0] [:parent 1] [:parent 2]]]
             (run parent "[(vec xs) @events]")))
      (let [grandchild (sci/fork child)]
        (is (= [[[:parent 0] [:child 1] [:child 2]] 3]
               (run grandchild "[(vec xs) (count @events)]")))))))

(deftest aliases-captured-in-closures-and-containers-remain-world-local
  (let [parent (forkable)]
    (run parent "(def counter (atom 0))
                 (def xs (lazy-seq (list (swap! counter inc))))
                 (def aliases {:one xs :nested [xs]})
                 (def read-captured (let [captured xs] (fn [] (first captured))))
                 nil")
    (let [child (sci/fork parent)]
      (is (= [1 1 1 1]
             (run child "[(read-captured)
                          (first (:one aliases))
                          (first (first (:nested aliases)))
                          @counter]")))
      (is (= 0 (run parent "@counter")))
      (is (= [1 1] (run parent "[(read-captured) @counter]")))
      (is (= 1 (run child "@counter"))))))

(deftest sequence-value-operations-match-standard-sci
  (doseq [code ["(let [xs (lazy-seq (list 1 2 3))]
                   [(seq? xs) (sequential? xs) (seq xs) (first xs) (next xs)
                    (rest xs) (count xs) (= xs '(1 2 3)) (= '(1 2 3) xs)
                    (= (hash xs) (hash '(1 2 3))) (conj xs 0) (empty xs)])"
                "(let [xs (lazy-seq nil)]
                   [(seq xs) (first xs) (next xs) (rest xs) (count xs)
                    (= xs '()) (= '() xs) (= (hash xs) (hash '()))])"
                "(let [calls (atom 0)
                       xs (lazy-seq (swap! calls inc) (list :value))
                       alias (with-meta xs {:tag :alias})]
                   [(meta xs) (meta alias) (realized? xs) (realized? alias)
                    @calls (first xs) (first alias) @calls])"]]
    (is (= (run (sci/init {}) code) (run (forkable) code)) code)))

(deftest hashing-and-equality-follow-world-local-realization
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (def counter (atom 0))
                 (def xs (lazy-seq (list [label (swap! counter inc)])))
                 nil")
    (let [child (sci/fork parent)]
      (is (= [true true 1]
             (run child "(def label :child)
                         [(= (hash xs) (hash '([:child 1])))
                          (= xs '([:child 1])) @counter]")))
      (is (= [false 0] (run parent "[(realized? xs) @counter]")))
      (is (= [true true 1]
             (run parent "[(= (hash xs) (hash '([:parent 1])))
                           (= xs '([:parent 1])) @counter]"))))))

(deftest metadata-aliases-inherit-the-prefix-with-world-local-tails
  (let [parent (forkable)]
    (run parent "(def events (atom []))
                 (defn cells [n]
                   (lazy-seq
                     (when (< n 3)
                       (swap! events conj n)
                       (cons n (cells (inc n))))))
                 (def xs (cells 0))
                 (def alias (with-meta xs {:origin :metadata-alias}))
                 nil")
    (let [child (sci/fork parent)]
      (is (= [[0 1 2] [0 1 2] {:origin :metadata-alias}]
             (run child "[(vec alias) @events (meta alias)]")))
      (is (= #?(:clj [0] :cljs [] :cljd [0]) (run parent "@events")))
      (is (= [[0 1 2] [0 1 2]] (run parent "[(vec xs) @events]"))))))

(deftest exception-and-normalization-semantics-match-standard-sci
  (let [forms [(str "(def calls (atom 0))
                     (def xs (lazy-seq
                               (if (= 1 (swap! calls inc))
                                 (throw (ex-info \"first attempt\" {}))
                                 (list :ok))))
                     [" (caught "(first xs)") " (realized? xs) @calls
                      " (caught "(first xs)") " (realized? xs) @calls]")
               (str "(def calls (atom 0))
                     (def xs (lazy-seq
                               (swap! calls inc)
                               (lazy-seq
                                 (swap! calls inc)
                                 (throw (ex-info \"normalizing\" {})))))
                     [" (caught "(first xs)") " (realized? xs) @calls
                      " (caught "(first xs)") " (realized? xs) @calls]")]]
    (doseq [code forms]
      (is (= (run (sci/init {}) code) (run (forkable) code)) code))))

(deftest a-fork-after-a-failed-thunk-retains-retry-semantics
  (let [parent (forkable)]
    (run parent "(def calls (atom 0))
                 (def xs (lazy-seq
                           (if (= 1 (swap! calls inc))
                             (throw (ex-info \"first attempt\" {}))
                             (list (swap! calls inc)))))
                 nil")
    (is (= :failed (run parent (caught "(first xs)"))))
    (let [child (sci/fork parent)]
      (is (= [3 3] (run child "[(first xs) @calls]")))
      (is (= 1 (run parent "@calls")))
      (is (= [3 3] (run parent "[(first xs) @calls]"))))))

(deftest genuinely-infinite-sequences-stay-lazy
  (let [parent (forkable)]
    (run parent "(def counter (atom 0))
                 (defn ticks []
                   (lazy-seq (cons (swap! counter inc) (ticks))))
                 (def xs (ticks))
                 nil")
    (let [child (sci/fork parent)]
      (is (= [[1 2 3 4] 4] (run child "[(vec (take 4 xs)) @counter]")))
      (is (= 0 (run parent "@counter")))
      (is (= [[1 2] 2] (run parent "[(vec (take 2 xs)) @counter]")))
      (is (= 4 (run child "@counter"))))))

(deftest chunk-effects-match-standard-sci
  (doseq [code ["(let [calls (atom [])
                        xs (map (fn [x] (swap! calls conj x) x) (vec (range 70)))]
                    [(first xs) (count @calls) (vec (take 35 xs)) (count @calls)])"
                "(let [calls (atom [])
                        xs (filter (fn [x] (swap! calls conj x) (odd? x))
                                   (vec (range 70)))]
                    [(first xs) (count @calls) (vec (take 18 xs)) (count @calls)])"]]
    (is (= (run (sci/init {}) code) (run (forkable) code)) code)))

(deftest host-consumption-preserves-the-returning-world
  (let [parent (forkable)]
    (run parent "(def counter (atom 0))
                 (def label :parent)
                 (def xs (lazy-seq (list [label (swap! counter inc)])))
                 nil")
    (let [child (sci/fork parent)
          returned (run child "(def label :child) xs")
          contained (run child "{:items [xs]}")]
      (is (= [:child 1] (first returned)))
      (is (= [:child 1] (first (first (:items contained)))))
      (is (= 1 (run child "@counter")))
      (is (= 0 (run parent "@counter")))
      (is (= [:parent 1] (first (run parent "xs")))))))

(deftest interpreted-functions-in-returned-sequences-keep-child-provenance
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (defn observe [] label)
                 (def xs (lazy-seq (list observe observe observe)))
                 (def nested-xs
                   (lazy-seq (list {:function observe :nested [observe]})))
                 (def chunked-xs (map (fn [_] observe) (vec (range 40))))
                 (first xs)
                 nil")
    (let [child (sci/fork parent)
          _ (run child "(def label :child)")
          returned (run child "xs")
          nested (run child "nested-xs")
          chunked (run child "chunked-xs")]
      (testing "first, next and rest project existing interpreted functions"
        (is (= :child ((first returned))))
        (is (= :child ((first (next returned)))))
        (is (= :child ((first (rest returned))))))
      (testing "projection descends through finite element maps and vectors"
        (let [element (first nested)]
          (is (= :child ((:function element))))
          (is (= :child ((first (:nested element)))))))
      (testing "metadata aliases retain the returning context and projection"
        (let [alias (with-meta returned {:returning :child})]
          (is (= {:returning :child} (meta alias)))
          (is (= :child ((first alias))))))
      (testing "chunk access projects functions as ordinary element access does"
        (let [s (seq chunked)]
          (is (chunked-seq? s))
          (when (chunked-seq? s)
            (let [chunk (chunk-first s)]
              (is (= :child ((nth chunk 0))))
              (is (= :child ((nth chunk (dec (count chunk))))))))))
      (is (= :parent ((first (run parent "xs"))))
          "projecting a child result must not replace the shared cached function"))))

(defn- assert-returned-sequence-laws [value]
  (let [head (first value)
        snapshot (vec value)
        metadata-alias (with-meta value {:view :alias})]
    (is (identical? head (first value)) "reading a cached head preserves its identity")
    (is (identical? head (first (seq value))) "seq retains the same projected head")
    (is (identical? head (first snapshot)) "iteration retains the same projected head")
    (is (identical? head (first metadata-alias)) "metadata changes do not replace projected elements")
    (is (= value snapshot))
    (is (= snapshot value))
    (is (= (hash value) (hash snapshot)))
    #?(:clj (is (= (.hashCode ^Object value) (.hashCode ^Object snapshot))))
    (when (next value)
      (let [second-value (nth snapshot 1)]
        (is (identical? second-value (first (next value))))
        (is (identical? second-value (first (rest value))))))
    (let [s (seq value)]
      (when (chunked-seq? s)
        (let [chunk (chunk-first s)]
          (is (identical? head (nth chunk 0)))
          (is (identical? head (nth (chunk-first (seq value)) 0)))
          (is (= (mapv #(nth chunk %) (range (count chunk)))
                 (subvec snapshot 0 (count chunk)))))))))

(deftest projected-function-elements-obey-collection-laws
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (defn observe [] label)
                 (def xs (lazy-seq (list observe observe observe)))
                 (def nested-xs (lazy-seq (list {:function observe :nested [observe]})))
                 (def chunked-xs (map (fn [_] observe) (vec (range 40))))
                 nil")
    (let [child (sci/fork parent)]
      (run child "(def label :child)")
      (doseq [expression ["xs" "nested-xs" "chunked-xs"]]
        (testing expression
          (assert-returned-sequence-laws (run child expression))))
      (is (= :child ((first (run child "xs")))))
      (is (= :parent ((first (run parent "xs"))))))))

(deftest host-consumption-can-select-a-related-world-explicitly
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (def xs (lazy-seq (list label)))
                 nil")
    (let [from-parent (run parent "xs")
          child (sci/fork parent)]
      (run child "(def label :child)")
      (is (= :child (sci/call-with-context child #(first from-parent))))
      (is (= false (run parent "(realized? xs)")))
      (is (= :parent (first from-parent))))))

(deftest an-unrelated-world-does-not-retarget-a-returned-sequence
  (let [parent (forkable)]
    (run parent "(def label :owner)
                 (def counter (atom 0))
                 (def xs (lazy-seq (list [label (swap! counter inc)])))
                 nil")
    (let [returned (run parent "xs")
          unrelated (sci/init {:runtime-mode :forkable
                               :bindings {'foreign returned}})]
      (is (= [:owner 1]
             (run unrelated "(def label :unrelated)
                             (def counter (atom 100))
                             (first foreign)")))
      (is (= 1 (run parent "@counter")))
      (is (= 100 (run unrelated "@counter"))))))

(deftest standard-mode-keeps-native-sequence-values
  (let [xs (run (sci/init {}) "(map inc [1 2 3])")]
    #?(:clj (is (instance? clojure.lang.LazySeq xs))
       :cljs (is (instance? cljs.core/LazySeq xs))
       :cljd (is (= '(2 3 4) xs)))
    (is (= '(2 3 4) xs))))

#?(:cljd nil
   :clj
   (deftest foreign-host-lazy-sequences-remain-affine
     (let [parent (sci/init {:runtime-mode :forkable})]
       (sci/intern parent 'user 'foreign (map inc [1 2 3]))
       (try
         (sci/fork parent)
         (is false "a host lazy cache must not be silently shared")
         (catch clojure.lang.ExceptionInfo e
           (is (= :lazy-seq (:resource-kind (ex-data e)))))))))

#?(:cljd nil
   :clj
   (deftest concurrent-realization-runs-the-thunk-once
     (let [entered (promise)
           release (promise)
           calls (atom 0)
           ctx (sci/init {:runtime-mode :forkable
                          :bindings {'block! (fn []
                                               (swap! calls inc)
                                               (deliver entered true)
                                               @release)}})]
       (run ctx "(def xs (lazy-seq (block!) (list :value))) nil")
       (let [first-reader (future (run ctx "(first xs)"))
             second-reader (future (run ctx "(first xs)"))]
         (try
           (is (= true (deref entered 2000 ::timeout)))
           (is (= ::waiting (deref second-reader 50 ::waiting)))
           (deliver release true)
           (is (= :value (deref first-reader 2000 ::timeout)))
           (is (= :value (deref second-reader 2000 ::timeout)))
           (is (= 1 @calls))
           (finally
             (deliver release true)
             (future-cancel first-reader)
             (future-cancel second-reader)))))))

#?(:cljd nil
   :clj
   (deftest fork-waits-for-an-active-realization
     (let [entered (promise)
           release (promise)
           calls (atom 0)
           parent (sci/init {:runtime-mode :forkable
                             :bindings {'block! (fn []
                                                  (swap! calls inc)
                                                  (deliver entered true)
                                                  @release)}})]
       (run parent "(def xs (lazy-seq (block!) (list :value))) nil")
       (let [reader (future (run parent "(first xs)"))]
         (try
           (is (= true (deref entered 2000 ::timeout)))
           (let [forking (future (sci/fork parent))]
             (try
               (is (= ::waiting (deref forking 50 ::waiting)))
               (deliver release true)
               (is (= :value (deref reader 2000 ::timeout)))
               (let [child (deref forking 2000 ::timeout)]
                 (is (map? child))
                 (when (map? child)
                   (is (= :value (run child "(first xs)")))))
               (is (= 1 @calls) "child inherits the completed realization")
               (finally (future-cancel forking))))
           (finally
             (deliver release true)
             (future-cancel reader)))))))


(deftest lazy-body-retains-its-implicit-zero-argument-recur-target
  ;; Standard SCI historically has a separate implicit-recur limitation here.
  ;; Compare the managed path to Clojure's zero-argument lazy thunk contract,
  ;; rather than requiring an unrelated change to standard evaluation.
  (is (= [3 3]
         (run (forkable)
              "(def calls (atom 0))
               (def xs (lazy-seq
                         (if (< (swap! calls inc) 3)
                           (recur)
                           (list @calls))))
               [(first xs) @calls]")))
  (testing "the implicit thunk's recur target does not become the outer loop"
    (is (= [[:outside 3] 3]
           (run (forkable)
                "(def calls (atom 0))
                 (def xs
                   (loop [outer :outside]
                     (lazy-seq
                       (if (< (swap! calls inc) 3)
                         (recur)
                         (list outer @calls)))))
                 [(vec xs) @calls]")))))

(deftest lazy-body-captures-lexical-bindings-before-a-fork
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (def xs (let [fixed :lexical n 7]
                           (lazy-seq (list [fixed n label]))))
                 nil")
    (let [child (sci/fork parent)]
      (is (= [:lexical 7 :child]
             (run child "(def label :child) (first xs)")))
      (is (= [:lexical 7 :parent] (run parent "(first xs)"))))))

(deftest managed-lazy-helper-preserves-user-allowlists
  (let [code "(lazy-seq (list :allowed))"
        options {:allow '[lazy-seq list]}
        standard (sci/init options)
        managed (sci/init (assoc options :runtime-mode :forkable))]
    (is (= '(:allowed) (run standard code)))
    (is (= (run standard code) (run managed code)))
    (let [child (sci/fork managed)]
      (is (= '(:allowed) (run child code))))))


(deftest projected-tails-retained-before-a-fork-resolve-in-the-child
  (doseq [tail-kind [:next :rest :chunk-next]]
    (testing (name tail-kind)
      (let [parent (forkable)]
        (run parent "(def label :parent)
                     (defn observe [] label)
                     (def xs (map (fn [_] observe) (vec (range 40))))
                     nil")
        (let [returned (run parent "xs")
              tail (case tail-kind
                     :next (next returned)
                     :rest (rest returned)
                     :chunk-next (chunk-next (seq returned)))]
          ;; Materialize the parent projection before cloning its cache. A
          ;; child must not inherit a function already bound to the parent.
          (is (= :parent ((first tail))))
          (sci/intern parent 'user 'retained-tail tail)
          (let [child (sci/fork parent)]
            (run child "(def label :child)")
            (is (= :child ((first (run child "retained-tail")))))
            (is (= :child (sci/call-with-context child #((first tail))))))
          (is (= :parent ((first tail)))))))))

(deftest projected-public-sequences-do-not-pin-nested-lazy-bodies-to-the-parent
  (let [parent (forkable)]
    (run parent "(def label :parent)
                 (defn observe [] label)
                 (def xs (lazy-seq (list observe)))
                 nil")
    (let [public (run parent "xs")]
      (sci/intern parent 'user 'public-xs public)
      ;; Returning a public projection through another lazy body must normalize
      ;; to raw values, even after the parent has materialized its projection.
      (run parent "(def ys (lazy-seq public-xs)) (first ys) nil")
      (is (= :parent (run parent "((first ys))")))
      (let [child (sci/fork parent)]
        (run child "(def label :child)")
        (is (= :child (run child "((first ys))")))
        (is (= :parent (run parent "((first ys))")))))))

(deftest returned-functions-prefer-related-active-worlds
  (let [parent (forkable)]
    (run parent "(def label :parent) (defn observe [] label) nil")
    (let [public-function (run parent "observe")
          child (sci/fork parent)
          unrelated (forkable)]
      (run child "(def label :child)")
      (run unrelated "(def label :unrelated)")
      (sci/intern child 'user 'external public-function)
      (is (= :child (run child "(external)")))
      (is (= :child (sci/call-with-context child public-function)))
      (is (= :parent (sci/call-with-context unrelated public-function))
          "an unrelated active world cannot redirect a captured function")
      (is (= :parent (public-function))))))

#?(:clj
   (deftest standard-lazy-thunks-remain-retryable-after-an-exception
     ;; This is an explicit native contract, rather than a managed/standard
     ;; comparison: both paths could otherwise share the same broken retry.
     (is (= [:failed false 1 :ok true 2]
            (run (sci/init {})
                 "(def attempts (atom 0))
                  (def xs
                    (lazy-seq
                      (if (= 1 (swap! attempts inc))
                        (throw (ex-info \"first realization fails\" {}))
                        (list :ok))))
                  [(try (first xs) (catch Exception _ :failed))
                   (realized? xs)
                   @attempts
                   (first xs)
                   (realized? xs)
                   @attempts]")))))

(deftest lazy-sequence-equality-is-boolean-for-all-shapes
  (doseq [mode [:standard :forkable]
          [expression reference] [["(lazy-seq nil)" '()]
                                  ["(lazy-seq (list 1 2))" '(1 2)]]]
    (testing (str (name mode) " " expression)
      (let [ctx (sci/init (if (= :forkable mode) {:runtime-mode :forkable} {}))
            value (run ctx expression)
            nonsequence #?(:clj (Object.) :cljs (js-obj) :cljd #{:not-a-sequence})]
        (doseq [other ['() [] '(1) [1] '(1 2) [1 2]
                       '(1 2 3) [1 2 3] '(1 3) [1 3]
                       nil {} {:value 1} nonsequence]]
          (let [expected (= reference other)]
            (is (= expected (= value other))
                "sequence equality returns the native boolean for this shape")
            (is (= expected (= other value))
                "reversing the arguments preserves equality")
            #?(:clj
               (do
                 (is (= expected (.equals ^Object value other)))
                 (when (some? other)
                   (is (= expected (.equals ^Object other value))))))))))))
