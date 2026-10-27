(ns sci.lazy-foreign-input-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.fork :as fork]))

(defn- foreign-sequences []
  [[:lazy-seq (map inc [1 2 3])]
   [:iterate (iterate inc 0)]
   [:cycle (cycle [1 2 3])]
   [:iterator-seq #?(:clj (clojure.lang.IteratorSeq/create (.iterator ^java.lang.Iterable [1 2 3]))
                     :cljs (es6-iterator-seq (.values (js/Set. #js [1 2 3]))))]])

(deftest native-caching-and-consuming-sequences-are-explicitly-affine
  (doseq [[expected value] (foreign-sequences)]
    (testing (str expected)
      (is (= expected (fork/affine-resource-kind value)))
      (let [ctx (sci/init {:runtime-mode :forkable})]
        (try
          (sci/intern ctx 'user 'foreign value)
          (try
            (sci/fork ctx)
            (is false "a registered foreign sequence must not be shared implicitly")
            (catch #?(:clj Exception :cljs js/Error) error
              (is (= expected (:resource-kind (ex-data error))))))
          (finally (sci/retire! ctx)))))))

(deftest managed-producers-reject-foreign-inputs-before-capturing-them
  (doseq [[expected value] (foreign-sequences)]
    (testing (str expected)
      (let [ctx (sci/init {:runtime-mode :forkable})]
        (try
          (sci/intern ctx 'user 'foreign value)
          (doseq [form ["(map inc foreign)" "(filter odd? foreign)"
                        "(take 1 foreign)" "(concat [] foreign)"
                        "(cycle foreign)"]]
            (testing form
              (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                    #"explicit forkable adapter"
                                    (sci/eval-string* ctx form)))))
          ;; Removing the explicit host root permits an ordinary fork: no
          ;; hidden managed thunk containing that input was installed first.
          (sci/intern ctx 'user 'foreign nil)
          (let [child (sci/fork ctx)]
            (try
              (is (= [2 3] (sci/eval-string* child "(vec (map inc [1 2]))")))
              (finally (sci/retire! child))))
          (finally (sci/retire! ctx)))))))

(deftest replace-does-not-capture-a-mutable-host-substitution-map
  (let [ctx (sci/init {:runtime-mode :forkable})
        substitutions #?(:clj (java.util.HashMap. {1 :one})
                          :cljs (js/Map. #js [#js [1 "one"]]))]
    (try
      (sci/intern ctx 'user 'substitutions substitutions)
      (doseq [form ["(replace substitutions)"
                    "(replace substitutions (list 1 2))"
                    "(replace substitutions [1 2])"]]
        (testing form
          (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                               #"explicit forkable adapter"
                               (sci/eval-string* ctx form)))))
      (finally (sci/retire! ctx)))))

(deftest foreign-sequence-error-data-does-not-realize-the-rejected-value
  (let [calls (atom 0)
        infinite-input (map (fn [x] (swap! calls inc) x) (range))
        ctx (sci/init {:runtime-mode :forkable})]
    (try
      (sci/intern ctx 'user 'foreign infinite-input)
      (let [failure (try
                      (sci/fork ctx)
                      nil
                      (catch #?(:clj Exception :cljs js/Error) error error))
            data (ex-data failure)]
        (is (some? failure))
        (is (= :lazy-seq (:resource-kind data)))
        (is (not (contains? data :resource)))
        (is (string? (:resource-type data)))
        ;; Error reporting must terminate without even beginning this infinite
        ;; stream, rather than retaining it as a printable exception payload.
        (is (string? (pr-str data)))
        (is (zero? @calls)))
      (finally (sci/retire! ctx)))))
