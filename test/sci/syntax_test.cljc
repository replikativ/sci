(ns sci.syntax-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.namespaces :as namespaces]
            [sci.impl.syntax :as syntax]
            [sci.impl.vars :as vars]))

(defrecord OpaqueSyntax [payload])

(defn- context [mode]
  (sci/init (if (= :forkable mode) {:runtime-mode :forkable} {})))

(deftest resolved-native-macros-return-spliceable-syntax
  (let [ctx (context :forkable)]
    (try
      (sci/eval-string*
       ctx
       "(def ^:dynamic *syntax-value* :outside)
        (defn expand-direct [form]
          (apply (resolve (first form)) form {} (rest form)))")
      (doseq [[form expected]
              [["(let [x 1] (+ x 2))" 3]
               ["(loop [n 3 total 0] (if (zero? n) total (recur (dec n) (+ total n))))" 6]
               ["(binding [*syntax-value* :inside] *syntax-value*)" :inside]
               ["(-> {:n 1} (update :n inc) :n)" 2]
               ["(when true (let [x 2] (when (pos? x) (-> x inc))))" 3]]]
        (testing form
          (let [expanded (sci/eval-string* ctx (str "(expand-direct '" form ")"))]
            (is (= expected (sci/eval-form ctx expanded))))
          ;; This follows the direct resolved-macro path used by CPS code.
          ;; Merely macroexpanding and evaluating would miss a foreign host
          ;; LazySeq in the expansion's tail consumed by interpreted concat.
          (is (true?
               (sci/eval-string*
                ctx
                (str "(let [expanded (expand-direct '" form ")]
                        (= (rest expanded)
                           (rest `(syntax-wrapper ~@(rest expanded)))))"))))))
      (doseq [macro-name '[let loop binding -> when]]
        (is (true? (sci/eval-string*
                    ctx (str "(:macro (meta (resolve '" macro-name ")))")))))
      (finally (sci/retire! ctx)))))

(deftest canonicalization-preserves-collection-shapes-and-metadata
  (let [stable (with-meta {:unchanged [1 2]} {:stable true})
        cases [[:list (with-meta (list 'outer (lazy-seq (list 'x 'y)) stable) {:kind :list}) list?]
               [:vector (with-meta ['outer (lazy-seq (list 'x 'y)) stable] {:kind :vector}) vector?]
               [:map (with-meta {:code (lazy-seq (list 'x 'y)) :stable stable} {:kind :map}) map?]
               [:set (with-meta #{(lazy-seq (list 'x 'y)) stable} {:kind :set}) set?]]]
    (doseq [[kind form shape?] cases]
      (testing (name kind)
        (let [result (syntax/canonicalize form)]
          (is (shape? result))
          (is (= form result))
          (is (= (meta form) (meta result)))
          (is (not (identical? form result)))
          (is (identical? stable
                          (case kind
                            :list (nth result 2)
                            :vector (nth result 2)
                            :map (:stable result)
                            :set (get result stable)))))))
    (doseq [form ['(plain immutable syntax) [1 {:a 2}] {:a [1 2]} #{:a :b} stable]]
      (is (identical? form (syntax/canonicalize form))
          "immutable syntax without native lazy tails reuses its identity"))))

(deftest quoted-host-lazy-data-and-records-stay-opaque
  (doseq [quote-symbol ['quote 'clojure.core/quote]]
    (let [calls (atom 0)
          foreign (lazy-seq (swap! calls inc)
                            (throw (ex-info "quoted data must not run" {})))
          quoted (with-meta (list quote-symbol foreign) {:quoted true})
          record (->OpaqueSyntax foreign)]
      (is (identical? quoted (syntax/canonicalize quoted)))
      (is (identical? foreign (second (syntax/canonicalize quoted))))
      (is (identical? record (syntax/canonicalize record)))
      (is (identical? foreign (:payload (syntax/canonicalize record))))
      (is (zero? @calls))
      (is (false? (realized? foreign))))))

(deftest trusted-native-expansion-keeps-quoted-foreign-data-unforced
  (let [calls (atom 0)
        foreign (lazy-seq (swap! calls inc)
                          (throw (ex-info "quoted payload must remain opaque" {})))
        ctx (context :forkable)]
    (try
      (sci/intern ctx 'user 'quoted-payload-form (list 'quote foreign))
      (let [expanded
            (sci/eval-string*
             ctx
             "(let [form (list 'let [] quoted-payload-form)]
                (apply (resolve 'let) form {} (rest form)))")]
        (is (identical? foreign (sci/eval-form ctx expanded)))
        (is (zero? @calls))
        (is (false? (realized? foreign))))
      ;; Syntax adaptation cannot become a runtime foreign-sequence adapter.
      (sci/intern ctx 'user 'foreign-runtime-value foreign)
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                            #"explicit forkable adapter"
                            (sci/eval-string* ctx "(concat [] foreign-runtime-value)")))
      (is (zero? @calls))
      (is (false? (realized? foreign)))
      (finally (sci/retire! ctx)))))

(deftest only-forkable-core-macros-receive-the-native-syntax-adapter
  (let [standard (context :standard)
        managed (context :forkable)
        native-let (vars/getRawRoot (get namespaces/clojure-core 'let))]
    (try
      (is (identical? native-let (sci/eval-string* standard "(deref (resolve 'let))"))
          "standard contexts retain the native macro function fast path")
      (is (not (identical? native-let
                            (sci/eval-string* managed "(deref (resolve 'let))"))))
      (is (true? (sci/eval-string* standard "(:macro (meta (resolve 'let)))")))
      (is (true? (sci/eval-string* managed "(:macro (meta (resolve 'let)))")))
      (finally
        (sci/retire! standard)
        (sci/retire! managed)))))

(deftest interpreted-user-macros-run-their-effects-exactly-once
  (doseq [mode [:standard :forkable]]
    (testing (name mode)
      (let [ctx (context mode)]
        (try
          (is (= [9 1]
                 (sci/eval-string*
                  ctx
                  "(def expansion-count (atom 0))
                   (defmacro counted [expression]
                     (swap! expansion-count inc)
                     `(let [value# ~expression] value#))
                   [(counted 9) @expansion-count]")))
          (finally (sci/retire! ctx)))))))

#?(:cljd nil
   :default
   (deftest unchanged-opaque-keys-are-not-rehashed-by-canonicalization
     (doseq [kind [:map :set]]
       (testing (name kind)
         (let [hash-calls (atom 0)
               key #?(:clj (reify Object
                             (hashCode [_] (swap! hash-calls inc) 42))
                      :cljs (reify IHash
                              (-hash [_] (swap! hash-calls inc) 42)))
               form (case kind
                      :map (with-meta {key [:unchanged]} {:kind :map})
                      :set (with-meta #{key} {:kind :set}))
               before @hash-calls
               result (syntax/canonicalize form)]
           (is (identical? form result))
           (is (= before @hash-calls)
               "walking unchanged syntax must not invoke opaque user hashing"))))))
