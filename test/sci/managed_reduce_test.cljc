(ns sci.managed-reduce-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.fork :as fork]
            [sci.impl.lazy-seq :as lazyseq]))

(deftest managed-reductions-match-core-semantics
  (doseq [code ["(reduce + (range 1001))" "(reduce + 7 (range 1001))"
                "(reduce + (lazy-seq nil))" "(reduce + 7 (lazy-seq nil))"
                "(reduce + (lazy-seq (list 42)))"
                "(reduce (fn [a n] (if (= n 5) (reduced a) (+ a n))) 0 (range 100))"
                "(transduce (comp (map inc) (take 5)) + (range 100))"]]
    (let [ctx (sci/init {:runtime-mode :forkable})]
      (try
        (is (= (sci/eval-string code) (sci/eval-string* ctx code)) code)
        (finally (sci/retire! ctx))))))

(deftest early-termination-does-not-touch-the-next-tail
  (let [ctx (sci/init {:runtime-mode :forkable})]
    (try
      (is (= [:done 0]
             (sci/eval-string* ctx
                               "(def calls (atom 0))
                                (def xs (lazy-seq (cons 1 (lazy-seq (swap! calls inc) (list 2)))))
                                [(reduce (fn [_ _] (reduced :done)) nil xs) @calls]")))
      (finally (sci/retire! ctx)))))

(deftest reductions-keep-effects-and-projected-functions-in-the-selected-world
  (doseq [source ["(vec (range 70))" "(apply list (range 70))"]]
    (testing source
      (let [parent (sci/init {:runtime-mode :forkable})]
        (try
          (sci/eval-string* parent
                            (str "(def label :parent) (def effects (atom 0))
                                  (defn observe [] label)
                                  (def xs (map (fn [_] (swap! effects inc) observe) " source ")) nil"))
          (let [returned (sci/eval-string* parent "xs")
                _ (first returned)
                before (sci/eval-string* parent "@effects")
                child (sci/fork parent)]
            (try
              (sci/eval-string* child "(def label :child)")
              (is (= (vec (repeat 70 :child))
                     (sci/call-with-context child
                                            #(reduce (fn [values f] (conj values (f))) [] returned))))
              (is (= before (sci/eval-string* parent "@effects")))
              (is (= (vec (repeat 70 :parent))
                     (reduce (fn [values f] (conj values (f))) [] returned)))
              (finally (sci/retire! child))))
          (finally (sci/retire! parent)))))))

(deftest reduction-validates-a-foreign-tail-before-consuming-it
  (let [calls (atom 0)
        foreign (map (fn [n] (swap! calls inc) n) [2 3])
        ctx (sci/init {:runtime-mode :forkable
                       :namespaces {'host {'values (fn [] (cons 1 foreign))}}})]
    (try
      (sci/eval-string* ctx "(def xs (lazy-seq (host/values))) nil")
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"explicit forkable adapter"
                            (reduce + (sci/eval-string* ctx "xs"))))
      (is (zero? @calls))
      (finally (sci/retire! ctx)))))

#?(:clj
   (deftest reduction-refuses-an-iterator-tail-before-advancing-it
     (let [calls (atom 0)
           iterator (reify java.util.Iterator
                      (hasNext [_] true)
                      (next [_] (swap! calls inc)))
           foreign (clojure.lang.IteratorSeq/create iterator)
           ctx (sci/init {:runtime-mode :forkable
                          :namespaces {'host {'values (fn [] (cons 1 foreign))}}})]
       (try
         (sci/eval-string* ctx "(def xs (lazy-seq (host/values))) nil")
         (let [before @calls]
           (is (thrown-with-msg? Exception #"explicit forkable adapter"
                                 (reduce + (sci/eval-string* ctx "xs"))))
           (is (= before @calls)))
         (finally (sci/retire! ctx))))))

(deftest native-backed-reductions-remain-cancellable
  (let [cancelled? (atom false) calls (atom 0)
        ctx (sci/init {:runtime-mode :forkable
                       :interrupt-fn #(when @cancelled? (throw (ex-info "cancelled" {})))})]
    (try
      (let [value (sci/eval-string* ctx "(lazy-seq (vec (range 1000)))")]
        (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"cancelled"
                              (reduce (fn [acc n]
                                        (when (= 5 (swap! calls inc)) (reset! cancelled? true))
                                        (+ acc n)) 0 value)))
        (is (<= 5 @calls 32)))
      (finally (reset! cancelled? false) (sci/retire! ctx)))))

(deftest host-reduction-owns-its-world-through-the-reducing-callback
  (doseq [operation [sci/fork sci/retire!]]
    (let [ctx (sci/init {:runtime-mode :forkable})]
      (try
        (let [value (sci/eval-string* ctx "(range 3)")]
          (is (thrown? #?(:clj Exception :cljs js/Error)
                       (reduce (fn [_ _] (operation ctx)) nil value))))
        (is (= 42 (sci/eval-string* ctx "42")))
        (finally (sci/retire! ctx))))))

(deftest reducing-callback-restores-the-source-after-another-world-call
  (let [ctx (sci/init {:runtime-mode :forkable}) other (sci/init {:runtime-mode :forkable})]
    (try
      (let [value (sci/eval-string* ctx "(def label :source) (defn observe [] label) (repeat 70 observe)")]
        (is (= (vec (repeat 70 :source))
               (reduce (fn [values f]
                         (sci/eval-string* other "(def label :other) label")
                         (conj values (f))) [] value))))
      (finally (sci/retire! other) (sci/retire! ctx)))))

(deftest retired-world-reduction-refuses-before-running-the-callback
  (let [ctx (sci/init {:runtime-mode :forkable}) calls (atom 0)
        value (sci/eval-string* ctx "(range 3)")]
    (sci/retire! ctx)
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"retired"
                          (reduce (fn [acc n] (swap! calls inc) (+ acc n)) 0 value)))
    (is (zero? @calls))))

(deftest oversized-native-chunks-check-cancellation-within-the-chunk
  (let [cancelled? (atom false) calls (atom 0)
        buffer (chunk-buffer 128)
        _ (doseq [n (range 128)] (chunk-append buffer n))
        values (chunk-cons (chunk buffer) nil)
        ctx (sci/init {:runtime-mode :forkable
                       :interrupt-fn #(when @cancelled? (throw (ex-info "cancelled" {})))
                       :namespaces {'host {'values (constantly values)}}})]
    (try
      (let [value (sci/eval-string* ctx "(lazy-seq (host/values))")]
        (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"cancelled"
                              (reduce (fn [acc n]
                                        (when (= 5 (swap! calls inc)) (reset! cancelled? true))
                                        (+ acc n)) 0 value)))
        (is (<= 5 @calls 32)))
      (finally (reset! cancelled? false) (sci/retire! ctx)))))

#?(:clj
   (deftest explicit-adapters-bypass-native-resource-probes
     (let [value (reify
                   fork/Forkable
                   (fork-value [this] this)
                   java.util.concurrent.Future
                   (isDone [_] (throw (ex-info "Do not probe adapted Future" {})))
                   (isCancelled [_] false)
                   (cancel [_ _] false)
                   (get [_] (throw (UnsupportedOperationException.)))
                   (get [_ _ _] (throw (UnsupportedOperationException.))))]
       (is (identical? value (lazyseq/checked-input value))))))
