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
