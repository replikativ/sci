(ns sci.type-world-test
  "Where a SCI type's data lives under forks (babashka/sci#1132 keeps the
   built-in interface methods of deftype/defrecord in it). A call on an
   instance reads the active world's own definition, else that of the type's
   home (the world that evaluated the deftype), as for a Var. Analysis only
   declares a type. A write from a world without the data copies it first.
   Once the home is retired, user methods are refused, printing falls back
   to the default form, and record hash/= stay field-based."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]))

(defn- forkable [] (sci/init {:runtime-mode :forkable}))

(defn- attempt [f]
  (try (f)
       (catch #?(:clj Exception :cljs :default) e
         (if (re-find #"retired" (str (ex-message e)))
           :retired
           (str "EX " (ex-message e))))))

(def ^:private deftype-src
  #?(:clj "(deftype D [x]
             clojure.lang.IDeref (deref [_] x)
             clojure.lang.IFn (invoke [_ a] (+ x a))
             Object (toString [_] (str \"D:\" x)))"
     :cljs "(deftype D [x]
              IDeref (-deref [_] x)
              IFn (-invoke [_ a] (+ x a))
              Object (toString [_] (str \"D:\" x)))"))

(def ^:private use-src
  "(fn [x] [@x (x 1) (str x)])")

(defn- placements
  "Each returns [instance child]: D defined at the top level of a child, or
   by a fn the parent analysed (and defined) before the fork, evaluated in
   the child only."
  [p]
  {:top-level
   (fn []
     (let [c (sci/fork p)]
       [(sci/eval-string* c (str deftype-src " (->D 42)")) c]))
   :analysed-in-parent
   (fn []
     (sci/eval-string* p (str "(defn mk [] " deftype-src " (->D 42))"))
     (let [c (sci/fork p)]
       [(sci/eval-string* c "(mk)") c]))})

(deftest instance-methods-in-related-worlds-and-the-host
  (doseq [placement [:top-level :analysed-in-parent]]
    (testing (name placement)
      (let [p (forkable)
            older (sci/fork p)
            [d c] ((get (placements p) placement))
            newer (sci/fork p)
            descendant (sci/fork c)
            fns (into {} (for [[label ctx] [[:parent p] [:older older] [:newer newer]
                                            [:descendant descendant]]]
                           [label (sci/eval-string* ctx use-src)]))
            expected [42 43 "D:42"]]
        (doseq [[label f] fns]
          (is (= expected (attempt #(f d))) (str label " while the defining world is open")))
        (is (= expected (attempt #(sci/eval-string* c (str "(" use-src " (->D 42))")))) "child")
        ;; (a JVM host cannot deref a SCI deftype: it is not a host IDeref)
        (is (= #?(:clj [43 "D:42"] :cljs [42 43 "D:42"])
               (attempt #(vector #?@(:cljs [(deref d)]) (d 1) (str d))))
            "host")
        (sci/retire! c)
        (doseq [label [:parent :older :newer]]
          (is (= :retired (attempt #((get fns label) d))) (str label " after retire")))
        (is (= :retired (attempt #(d 1))) "host invoke after retire")
        (is (= :retired (attempt #(str d))) "host str after retire")
        (is (re-find #"retired SCI world" (str (attempt #(pr-str d))))
            "printing falls back to the default form")
        (is (= expected (attempt #((:descendant fns) d)))
            "a descendant keeps its own copy")))))

(deftest a-write-from-a-world-without-the-data-copies-it
  ;; The parent changes the deref of a type defined in its child. Only the
  ;; parent's own copy changes: the child, an older sibling and the host
  ;; are unaffected.
  (let [p (forkable)
        older (sci/fork p)
        c (sci/fork p)
        d (sci/eval-string* c (str deftype-src " (->D 42)"))
        t (sci/eval-string* c "D")
        older-f (sci/eval-string* older "(fn [x] @x)")
        change (sci/eval-string*
                p #?(:clj "(fn [t] (alter-meta! t assoc :foo 1)
                                   (defmethod clojure.lang/deref t [_] :from-parent) nil)"
                     :cljs "(fn [t] (alter-meta! t assoc :foo 1)
                                    (extend-type t IDeref (-deref [_] :from-parent)) nil)"))]
    (change t)
    (is (= 42 (sci/eval-string* c "@(->D 42)")) "child")
    (is (= 42 (sci/eval-string* c "(deref (first [(->D 42)]))")) "child, new instance")
    (is (= 42 (older-f d)) "older sibling")
    #?(:cljs (is (= 42 (deref d)) "host"))
    #?(:clj (is (= :from-parent ((sci/eval-string* p "(fn [x] @x)") d))
                "the parent sees its change (JVM: definitions resolve where invoked)"))))

(deftest records-after-their-world-is-retired
  (let [p (forkable)
        c (sci/fork p)
        r (sci/eval-string*
           c #?(:clj "(defrecord R [a] Object (toString [_] \"R!\") clojure.lang.IFn (invoke [_ x] x)) (->R 1)"
                :cljs "(defrecord R [a] Object (toString [_] \"R!\") IFn (-invoke [_ x] x)) (->R 1)"))
        r2 (sci/eval-string* c "(->R 1)")
        h (hash r)]
    (is (= ["R!" 2] [(str r) (r 2)]))
    (sci/retire! c)
    (is (= [h true 1] [(hash r) (= r r2) (get {r 1} r2)])
        "hash and = are field-based")
    (is (re-find #"\{:a 1\}" (str (attempt #(pr-str r)))) "printing works")
    #?(:clj (is (= :retired (attempt #(str r))) "a user toString is refused"))
    (is (= :retired (attempt #(r 2))) "a user method is refused")))

(deftest a-fork-between-analysis-and-definition-keeps-the-declaration
  ;; The parent analysed the deftype (inside mk) but defines it only when it
  ;; runs mk, after forking g. g keeps the bare declaration; a call in g on
  ;; an instance the parent made uses the parent's definition.
  (let [p (forkable)
        _ (sci/eval-string* p (str "(defn mk [] " deftype-src " (->D 42))"))
        g (sci/fork p)
        d (sci/eval-string* p "(mk)")]
    (is (= [42 43 "D:42"] ((sci/eval-string* g use-src) d)))
    #?(:clj (is (nil? (sci/eval-string* g "(:clojure.lang/deref (meta D))"))
                "g's own data is the declaration"))))

#?(:cljs
   (deftest a-user-printer-is-not-called-once-the-home-is-retired
     (let [p (forkable)
           c (sci/fork p)
           d (sci/eval-string* c "(deftype P [x] IPrintWithWriter (-pr-writer [_ w _] (-write w \"<P>\"))) (->P 1)")
           r (sci/eval-string* c "(defrecord Q [a] IPrintWithWriter (-pr-writer [_ w _] (-write w \"<Q>\"))) (->Q 1)")]
       (is (= ["<P>" "<Q>"] [(pr-str d) (pr-str r)]))
       (sci/retire! c)
       (is (re-find #"retired SCI world" (pr-str d)))
       (is (= "#user.Q{:a 1}" (pr-str r)))
       (is (re-find #"retired SCI world" (pr-str {:d d}))))))

(deftest a-delayed-deftype-defines-the-type-it-was-analysed-with
  (testing "redefined with another shape before the delayed definition runs"
    (let [p (forkable)
          _ (sci/eval-string* p (str "(defn mk [] " deftype-src " (->D 42))"))
          _ (sci/eval-string* p "(deftype D [x y])")
          c (sci/fork p)]
      (is (= [42 43 "D:42"] (sci/eval-string* c (str "(" use-src " (mk))"))))))
  (testing "called from another namespace"
    (let [p (forkable)
          _ (sci/eval-string* p (str "(ns foo) (defn mk [] " deftype-src " (->D 42)) (ns user)"))
          c (sci/fork p)]
      (is (= [42 43 "D:42"]
             (sci/eval-string* c (str "(ns bar) (" use-src " (foo/mk))"))))
      (is (sci/eval-string* c "(some? (get (ns-map 'foo) 'D))") "published in its own namespace"))))

(deftest a-declaration-does-not-claim-the-home
  ;; merge-opts registers the parent's declared type: it must not become the
  ;; home, so the host uses the child's definition
  (let [p (forkable)
        _ (sci/eval-string* p (str "(defn mk [] " deftype-src " (->D 42))"))
        _ (sci/merge-opts p {})
        c (sci/fork p)
        d (sci/eval-string* c "(mk)")]
    (is (= "D:42" (str d)))))

#?(:cljs
   (deftest cljs-prototypes-of-declared-types
     (testing "a fork of a world that holds a foreign instance of a type it only declared"
       (let [p (forkable)
             _ (sci/eval-string* p (str "(defn mk [] " deftype-src " (->D 42))"))
             c (sci/fork p)
             d (sci/eval-string* c "(mk)")
             _ ((sci/eval-string* p "(fn [x] (def held x) nil)") d)
             g (sci/fork p)]
         (is (= [42 43 "D:42"] (sci/eval-string* g (str "(" use-src " held)"))))))
     (testing "a record extended before its definition keeps record behaviour"
       (let [p (forkable)]
         (is (= [1 true :ext]
                (sci/eval-string*
                 p "(defn mk [] (defrecord R [a]) (->R 1))
                    (extend-type R IDeref (-deref [_] :ext))
                    (let [r (mk)] [(:a r) (record? r) @r])")))))))

(deftest a-mutable-instance-constructed-where-the-type-is-only-declared
  ;; The parent only declares C (mk analysed there, run in the child) and
  ;; constructs an instance with the child's factory: its mutable field is
  ;; still managed per world, so a fork of the parent has its own counter.
  (let [p (forkable)
        _ (sci/eval-string*
           p #?(:clj "(defn mk [] (deftype C [^:volatile-mutable n]
                                    clojure.lang.IDeref (deref [_] (set! n (inc n)) n))
                                  ->C)"
                :cljs "(defn mk [] (deftype C [^:mutable n]
                                     IDeref (-deref [_] (set! n (inc n)) n))
                                   ->C)"))
        c (sci/fork p)
        factory (sci/eval-string* c "(mk)")
        _ ((sci/eval-string* p "(fn [f] (def v [(f 0)]) nil)") factory)
        bump "@(first v)"
        first-parent (sci/eval-string* p bump)
        g (sci/fork p)]
    (is (= [1 2 2] [first-parent (sci/eval-string* g bump) (sci/eval-string* p bump)]))))

#?(:cljs
   (deftest cljs-fork-copies-and-printing-with-retired-worlds
     (testing "a fork of a world without the type's slot keeps the construction prototype"
       (let [p (forkable)
             o (sci/fork p)
             c (sci/fork p)
             d (sci/eval-string* c (str deftype-src " (->D 42)"))]
         ((sci/eval-string* o "(fn [x] (def held x) nil)") d)
         (sci/retire! c)
         (let [g (sci/fork o)]
           (is (some? (sci/eval-string* g "held")))
           (is (re-find #"retired SCI world" (pr-str (sci/eval-string* g "held")))))))
     (testing "a printer made in a retired child: its refusal propagates (the home is open)"
       (let [p (forkable)
             _ (sci/eval-string* p "(deftype P [x])")
             c (sci/fork p)
             ;; the method reads an atom of the child (captured: a CLJS
             ;; JIT-compiled fn called from the host does not enter its
             ;; world, so a Var read would not see the child's value)
             d (sci/eval-string*
                c "(let [tag (atom \"<P>\")]
                     (extend-type P IPrintWithWriter (-pr-writer [_ w _] (-write w @tag))))
                   (->P 1)")]
         (is (= "<P>" (pr-str d)))
         (sci/retire! c)
         (is (= :retired (attempt #(pr-str d))))))))

(deftest printing-and-expansions-behave-as-upstream
  (testing "an application exception tagged like a retirement propagates"
    (let [ctx (forkable)]
      (is (= "EX app"
             (attempt #(sci/eval-string*
                        ctx "(deftype T [] Object (toString [_] (throw (ex-info \"app\" {:type :sci.impl.world/retired-world}))))
                             (pr-str (T.))"))))))
  (testing "a defrecord expansion is data: evaluated twice it defines two types"
    (doseq [ctx [(sci/init {}) (forkable)]]
      (let [exp (sci/eval-string* ctx "(macroexpand-1 '(defrecord Q [a]))")
            t1 (sci/eval-form ctx exp)
            t2 (sci/eval-form ctx exp)]
        (is (not (identical? t1 t2)))
        (is (= 1 (sci/eval-string* ctx "(:a (->Q 1))")))))))

#?(:clj
   (deftest a-user-printer-writes-straight-to-the-writer
     ;; no staging: what a print-method wrote is in the destination while it
     ;; runs (as in .9 and upstream), so output size is not bounded by memory
     (let [sw (java.io.StringWriter.)
           p (sci/init {:runtime-mode :forkable
                        :namespaces {'host {'seen #(count (str sw))}}})
           r (sci/eval-string* p "(defrecord L []) (defmethod print-method L [_ w] (.write w \"abc\") (.write w (str (host/seen)))) (->L)")]
       (print-method r sw)
       (is (= "abc3" (str sw))))))

#?(:clj
   (deftest a-local-definition-counts-before-any-home
     ;; a type from a :standard context has no home in the forkable lineage;
     ;; a child's own print-method on it is used in that child only
     (let [s (sci/init {})
           d (sci/eval-string* s "(deftype D [x]) (->D 1)")
           t (sci/eval-string* s "D")
           p (sci/init {:runtime-mode :forkable :namespaces {'user {:types {'D t}}}})
           c (sci/fork p)]
       (sci/eval-string* c "(defmethod print-method D [_ w] (.write w \"local\"))")
       (is (= "local" ((sci/eval-string* c "(fn [x] (pr-str x))") d)))
       (is (re-find #"^#object\[user.D" (pr-str d)) "the standard context is unaffected"))))
