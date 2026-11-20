(ns sci.nonblocking-fork-test
  "Forks and retirement never wait for an evaluation; a fork is a consistent
   cut between individual publications."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.addons :as addons]
            [sci.core :as sci]
            [sci.impl.world :as world]))

(defn- forkable [opts]
  (sci/init (assoc opts :runtime-mode :forkable)))

(defn- within
  "Run `f` on another thread; its value, or ::timeout after `ms`."
  [ms f]
  (let [fut (future (f))
        v (deref fut ms ::timeout)]
    (when (= ::timeout v) (future-cancel fut))
    v))

(defn- retired-error? [e]
  (boolean (and e (re-find #"retired" (str (ex-message e))))))

(defn- failure [f]
  (try (f) nil (catch Throwable e e)))

(deftest fork-does-not-wait-for-a-blocked-evaluation
  (let [entered (promise)
        release (promise)
        ctx (forkable {:namespaces {'host {'entered #(deliver entered true)
                                           'wait #(deref release 10000 :timeout)}}})
        _ (sci/eval-string* ctx "(def a (atom 1)) (def x 1)")
        evaluation (future (sci/eval-string*
                            ctx "(swap! a inc) (host/entered) (host/wait) (def x 2) [x @a]"))]
    (try
      (is (true? (deref entered 5000 ::timeout)))
      (let [child (within 5000 #(sci/fork ctx))]
        (is (map? child) "the fork completed while the evaluation was blocked")
        (when (map? child)
          (is (= [1 2] (sci/eval-string* child "[x @a]"))
              "the child has the writes published before the fork")
          (is (= 3 (sci/eval-string* child "(swap! a inc)")))))
      (deliver release :released)
      (is (= [2 2] (deref evaluation 5000 ::timeout)))
      (finally (deliver release :released)))))

(deftest fork-from-inside-an-evaluation-of-the-same-interpreter
  (let [self (atom nil)
        children (atom [])
        ctx (forkable {:namespaces {'host {'fork! #(let [c (sci/fork @self)]
                                                       (swap! children conj c)
                                                       (count @children))
                                           'eval-in-child (fn [s] (sci/eval-string* (peek @children) s))}}})]
    (reset! self ctx)
    (is (= [1 :b]
           (sci/eval-string* ctx "(def a 1) (def counter (atom 10)) (def n (host/fork!)) (def b :b) [n b]")))
    (let [child (first @children)]
      (is (= [1 10 false] (sci/eval-string* child "[a @counter (contains? (ns-interns 'user) 'b)]"))
          "the cut is between the defs: a and counter, not n or b")
      (is (= 11 (sci/eval-string* child "(swap! counter inc)")))
      (is (= 10 (sci/eval-string* ctx "@counter"))))
    (testing "the child can be evaluated from inside the parent's evaluation"
      (is (= 12 (sci/eval-string* ctx "(host/fork!) (swap! counter + 2) (host/eval-in-child \"(swap! counter + 2)\")")))
      (is (= 12 (sci/eval-string* ctx "@counter"))))
    (testing "a lazy sequence being realized when its interpreter forks"
      (is (= (range 1 11)
             (sci/eval-string*
              ctx "(def xs (map (fn [i] (when (= i 5) (host/fork!)) (inc i)) (range 10))) (doall xs)")))
      (is (= (range 1 11) (sci/eval-string* (peek @children) "(doall xs)"))))))

(deftest retirement-closes-now-and-in-flight-evaluations-finish
  (let [entered (promise)
        release (promise)
        ctx (forkable {:namespaces {'host {'entered #(deliver entered true)
                                           'wait #(deref release 10000 :timeout)}}})
        _ (sci/eval-string* ctx "(def a (atom 1)) (def xs (map inc (range 5)))")
        evaluation (future
                     (sci/eval-string*
                      ctx "(host/entered) (host/wait)
                           (def b (atom (swap! a inc)))
                           (def ys (map inc (range 3)))
                           [@a @b (vec xs) (vec ys)]"))]
    (try
      (is (true? (deref entered 5000 ::timeout)))
      (is (nil? (within 5000 #(sci/retire! ctx))) "retirement did not wait")
      (is (retired-error? (failure #(sci/eval-string* ctx "1"))) "new evaluations are refused")
      (is (retired-error? (failure #(sci/fork ctx))))
      (deliver release :released)
      (is (= [2 2 [1 2 3 4 5] [1 2 3]] (deref evaluation 5000 ::timeout))
          "the running evaluation finished against its world")
      (finally (deliver release :released)))))

(deftest retirement-from-inside-its-own-evaluation
  (let [self (atom nil)
        ctx (forkable {:namespaces {'host {'retire! #(sci/retire! @self)}}})]
    (reset! self ctx)
    (is (= 3 (sci/eval-string* ctx "(def a (atom 1)) (host/retire!) (swap! a inc) (inc @a)")))
    (is (retired-error? (failure #(sci/eval-string* ctx "1"))))))

(deftest q7-user-code-under-alter-meta-does-not-block-a-swap
  ;; The reviewer's q7: A's alter-meta! fn waits for C; B's allocation needs a
  ;; resize; C's swap! must not queue behind it.
  (let [p (promise)
        entered (promise)
        ctx (forkable {:namespaces {'host {'wait #(deref p 4000 :wait-timeout)
                                           'entered #(deliver entered true)
                                           'release #(deliver p :released)}}})]
    (sci/eval-string* ctx "(def x 1) (def a (atom 0))")
    (let [A (future (sci/eval-string* ctx "(alter-meta! #'x (fn [m] (host/entered) (assoc m :w (host/wait))))"))
          _ (deref entered 5000 ::timeout)
          B (future (sci/eval-string* ctx "(count (doall (map (fn [i] (atom i)) (range 400))))"))
          _ (Thread/sleep 200)
          C (future (sci/eval-string* ctx "(swap! a inc) (host/release) :c-done"))]
      (is (= :c-done (deref C 2000 ::timeout)))
      (is (= 400 (deref B 5000 ::timeout)))
      (is (= :released (:w (deref A 5000 ::timeout))) "A's fn was released by C, not by its timeout")
      (is (= :released (sci/eval-string* ctx "(:w (meta #'x))"))))))

(deftest a-fork-does-not-wait-for-user-code-under-alter-meta
  (let [p (promise)
        entered (promise)
        ctx (forkable {:namespaces {'host {'wait #(deref p 4000 :wait-timeout)
                                           'entered #(deliver entered true)}}})]
    (sci/eval-string* ctx "(def x 1)")
    (let [A (future (sci/eval-string* ctx "(alter-meta! #'x (fn [m] (host/entered) (assoc m :w (host/wait))))"))]
      (deref entered 5000 ::timeout)
      (let [child (within 2000 #(sci/fork ctx))]
        (is (map? child))
        (when (map? child)
          (is (nil? (sci/eval-string* child "(:w (meta #'x))")))))
      (deliver p :released)
      (is (= :released (:w (deref A 5000 ::timeout)))))))

;;;; Consistent cut under concurrent publications

(defn- registered? [ctx handle]
  (boolean (world/registered? (:sci.impl/world ctx) handle)))

(defn- check-cut
  "Problems with the state a child was forked with; empty when consistent."
  [child]
  (let [env @(:env child)
        problems (transient [])]
    (doseq [[ns-sym ns-map] (:namespaces env)
            :let [ns-obj (:obj ns-map)]]
      (when (and ns-obj (not (registered? child ns-obj)))
        (conj! problems [:unregistered-namespace ns-sym]))
      (doseq [[sym v] ns-map
              :when (instance? sci.lang.Var v)]
        (when-not (registered? child v)
          (conj! problems [:unregistered-var ns-sym sym])))
      (doseq [[sym t] (:types ns-map)
              :when (instance? sci.lang.Type t)]
        (when-not (registered? child t)
          (conj! problems [:unregistered-type ns-sym sym]))))
    (let [[x-value x-meta-i a b c methods vs nss]
          (sci/eval-string*
           child
           "[x (:i (meta #'x)) @a @b @c
             (vec (for [[k f] (methods m)] [k (f k)]))
             (vec (for [[s v] (ns-interns 'user) :when (re-matches #\"v[0-9]+\" (name s))]
                    [s (when (bound? v) @v)]))
             (vec (for [n (all-ns) :let [s (str (ns-name n))] :when (.startsWith s \"n.\")]
                    [s (some-> (ns-resolve n 'y) deref) (:ns-i (meta n))]))]")]
      (when-not (= x-value x-meta-i) (conj! problems [:var-value-meta x-value x-meta-i]))
      (when-not (<= b a) (conj! problems [:program-order a b]))
      (when-not (<= c a) (conj! problems [:causality a c]))
      (doseq [[k r] methods :when (not= k r)] (conj! problems [:method k r]))
      ;; A def's Var is interned (unbound) when analyzed and bound when
      ;; evaluated: a cut between the two sees it unbound, as in Clojure.
      (doseq [[s v] vs :when (and (some? v) (not= (str "v" v) (name s)))]
        (conj! problems [:var s v]))
      ;; create-ns, alter-meta! and intern are three publications.
      (doseq [[s y i] nss
              :when (or (and (some? i) (not= (str "n." i) s))
                        (and (some? y) (not= (str "n." y) s)))]
        (conj! problems [:ns s y i])))
    (persistent! problems)))

(deftest concurrent-publications-fork-into-consistent-cuts
  (let [ctx (forkable {})
        _ (sci/eval-string*
           ctx "(def ^{:i 0} x 0) (def a (atom 0)) (def b (atom 0)) (def c (atom 0))
                (def vol (volatile! 0)) (defmulti m identity)")
        stop (atom false)
        errors (atom [])
        writer (fn [f]
                 (future
                   (try
                     (loop [i 1]
                       (when (and (not @stop) (<= i 150))
                         (f i)
                         (recur (inc i))))
                     (catch Throwable t (swap! errors conj t)))))
        writers [(writer #(sci/eval-string* ctx (str "(def ^{:i " % "} x " % ")")))
                 (writer #(sci/eval-string* ctx (str "(def v" % " " % ") (def ^{:i " % "} x " % ")")))
                 (writer #(sci/eval-string* ctx (str "(create-ns 'n." % ") (alter-meta! (the-ns 'n." % ") assoc :ns-i " %
                                                     ") (intern 'n." % " 'y " % ")")))
                 (writer #(sci/eval-string* ctx (str "(deftype T" % " [f])")))
                 (writer #(sci/eval-string* ctx (str "(defmethod m " % " [k] k)")))
                 (writer #(sci/eval-string* ctx (str "(reset! a " % ") (reset! b " % ")")))
                 (writer (fn [_] (sci/eval-string* ctx "(reset! c @a) (vswap! vol inc)")))]
        problems (atom [])
        concurrent-forks (atom 0)]
    (try
      ;; Fork while the writers publish; check the children afterwards
      ;; (each is an independent cut).
      (let [children (loop [k 0 children []]
                       (if (and (< k 300) (not (every? realized? writers)))
                         (do (Thread/sleep 2)
                             (recur (inc k) (conj children (sci/fork ctx))))
                         children))]
        (reset! concurrent-forks (count children))
        (doseq [child children]
          (let [p (check-cut child)]
            (when (seq p) (swap! problems conj (take 5 p))))
          (sci/retire! child)))
      (finally
        (reset! stop true)
        (doseq [w writers] (deref w 10000 ::timeout))))
    (is (< 20 @concurrent-forks) "forks ran while the writers published")
    (is (empty? @errors) (pr-str (map ex-message @errors)))
    (is (empty? @problems) (pr-str (take 3 @problems)))
    (testing "the primary's host objects and slots agree once writers stopped"
      (let [child (sci/fork ctx)
            q "[x (meta #'x) @vol @a]"]
        (is (= (sci/eval-string* ctx q) (sci/eval-string* child q)))))))

(deftest a-lazy-sequence-realized-concurrently-with-forks
  (let [ctx (forkable {})
        _ (sci/eval-string* ctx "(def xs (map (fn [i] (* 2 i)) (range 20000)))")
        realizer (future (sci/eval-string* ctx "(reduce + xs)"))
        children (doall (for [_ (range 30)] (do (Thread/sleep 1) (sci/fork ctx))))
        expected (reduce + (map #(* 2 %) (range 20000)))]
    (is (= expected (deref realizer 10000 ::timeout)))
    (doseq [child children]
      (is (= expected (sci/eval-string* child "(reduce + xs)"))))))

(deftest a-delay-being-realized-forks-as-pending
  (let [entered (promise)
        release (promise)
        calls (atom 0)
        ctx (forkable {:namespaces {'host {'entered #(do (swap! calls inc) (deliver entered true))
                                           'wait #(deref release 10000 :timeout)}}})
        _ (sci/eval-string* ctx "(def d (delay (host/entered) (host/wait) :value))")
        realizer (future (sci/eval-string* ctx "@d"))]
    (try
      (is (true? (deref entered 5000 ::timeout)))
      (let [child (within 2000 #(sci/fork ctx))]
        (is (map? child) "the fork did not wait for the running thunk")
        (deliver release :released)
        (is (= :value (deref realizer 5000 ::timeout)))
        (when (map? child)
          (is (false? (sci/eval-string* child "(realized? d)")))
          (is (= :value (sci/eval-string* child "@d")) "the child realizes its own copy")
          (is (= 2 @calls)))
        (is (true? (sci/eval-string* ctx "(realized? d)")))
        (is (true? (sci/eval-string* (sci/fork ctx) "(realized? d)"))
            "a later fork copies the published realization"))
      (finally (deliver release :released)))))

(deftest a-running-future-in-world-state-is-refused-not-awaited
  ;; A fork no longer waits for a managed future's body: a still-running
  ;; Future held in the world is an affine host resource, as for any pending
  ;; Future; once it completed it is shared.
  (let [release (promise)
        ctx (forkable (addons/future {:namespaces {'host {'wait #(deref release 10000 :timeout)}}}))]
    (sci/eval-string* ctx "(def f (future (host/wait) :done))")
    (try
      (let [result (within 2000 #(try (sci/fork ctx) (catch Exception e e)))]
        (is (= :pending-future (:resource-kind (ex-data result)))))
      (deliver release :released)
      (is (= :done (sci/eval-string* ctx "@f")))
      (is (= :done (sci/eval-string* (sci/fork ctx) "@f")))
      (finally (deliver release :released)))))
