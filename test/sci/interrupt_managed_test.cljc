(ns sci.interrupt-managed-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.interrupt :as interrupt]))

(def ^:private producer-cases
  [["(range 5)" [0 1 2 3 4]]
   ["(range 1 6)" [1 2 3 4 5]]
   ["(range 1 10 2)" [1 3 5 7 9]]
   ["(take 5 (range))" [0 1 2 3 4]]
   ["(repeat 5 :x)" [:x :x :x :x :x]]
   ["(take 5 (repeat :x))" [:x :x :x :x :x]]
   ["(take 5 (cycle [1 2]))" [1 2 1 2 1]]
   ["(take 5 (iterate inc 0))" [0 1 2 3 4]]])

(defn- forkable-context [callback]
  (sci/init {:runtime-mode :forkable
             :interrupt-fn callback
             :namespaces {'clojure.core interrupt/clojure-core}}))

(deftest interrupt-producers-are-forkable-with-and-without-a-callback
  (doseq [callback [nil (fn [])]
          [source expected] producer-cases]
    (testing (str source " callback=" (boolean callback))
      (let [parent (forkable-context callback)]
        (sci/eval-string* parent
                          (str "(def calls (atom 0))"
                               "(def xs (map (fn [x] (swap! calls inc) x) " source "))"))
        (let [child (sci/fork parent)]
          (try
            (is (= expected (sci/eval-string* child "(vec xs)")))
            (is (= 5 (sci/eval-string* child "@calls")))
            (is (= 0 (sci/eval-string* parent "@calls")))
            (is (= expected (sci/eval-string* parent "(vec xs)")))
            (is (= 5 (sci/eval-string* parent "@calls")))
            (finally (sci/retire! child) (sci/retire! parent))))))))

(deftest managed-producer-realization-consults-the-current-consumer-callback
  (doseq [[source expected] producer-cases]
    (testing source
      (let [creator-calls (atom 0)
            consumer-calls (atom 0)
            parent (forkable-context #(swap! creator-calls inc))]
        (sci/eval-string* parent (str "(def xs " source ")"))
        (let [child (assoc (sci/fork parent)
                           :interrupt-fn #(swap! consumer-calls inc))
              value (sci/eval-string* child "xs")]
          (try
            ;; Only host-side realization follows these resets. Interpreter
            ;; analysis and fn-entry checks cannot account for these calls.
            (reset! creator-calls 0)
            (reset! consumer-calls 0)
            (is (= expected (vec value)))
            (is (pos? @consumer-calls))
            (is (zero? @creator-calls))
            (finally (sci/retire! child) (sci/retire! parent))))))))

(deftest consuming-without-a-callback-does-not-reuse-the-creators-callback
  (doseq [[source expected] producer-cases]
    (testing source
      (let [creator-calls (atom 0)
            parent (forkable-context #(swap! creator-calls inc))]
        (sci/eval-string* parent (str "(def xs " source ")"))
        (let [child (assoc (sci/fork parent) :interrupt-fn nil)
              value (sci/eval-string* child "xs")]
          (try
            (reset! creator-calls 0)
            (is (= expected (vec value)))
            (is (zero? @creator-calls))
            (finally (sci/retire! child) (sci/retire! parent))))))))
