(ns sci.fork-provenance-test
  "Copy-time fork policy never comes from mutable metadata. A Var's copier,
   which a fork runs inside its copy section under the source's locks, is
   installed only by SCI and the host (`:fork-fns`), never by `def`,
   `defonce`, `intern`, `reset-meta!` or `alter-meta!` followed by a root
   bind, unbind or watch, nor by upgrading an existing descriptor.
   `:sci/built-in` (write protection, a Var's home, whether a fork processes
   its value) is a trusted flag interpreted metadata can neither set nor
   clear. On CLJS, type metadata cannot replace the JS prototype the fixed
   type copier consumes."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.vars :as vars]))

(defn- spy-env
  "A host `spy` copier that records every call and poisons the value, and a
   host Var not registered in the world (only reachable through `boxed`)."
  []
  (let [calls (atom 0)
        spy (fn [_] (swap! calls inc) :poisoned)
        boxed-var (sci/new-var 'boxed-var 1 {:ns (sci/create-ns 'host)})]
    {:calls calls
     :boxed-var boxed-var
     :ctx (sci/init {:runtime-mode :forkable
                     :namespaces {'host {'spy spy 'boxed [boxed-var]}}})}))

(defn- each-route
  "Run `f` for one route; a throw is that route's failure, and the other
   routes still run."
  [label f]
  (testing label
    (try (f)
         (catch #?(:clj Exception :cljs :default) e
           (is false (str "threw: " (ex-message e)))))))

(defn- refused [ctx s]
  (try (sci/eval-string* ctx s) nil
       (catch #?(:clj Exception :cljs :default) e (ex-message e))))

(def ^:private metadata-routes
  "[label setup read]: `setup` puts `host/spy` as :sci.impl/fork-fn into Var
   metadata through one route; `read` reads that Var's value afterwards."
  [["def (twice)"
    "(def ^{:sci.impl/fork-fn host/spy} x 1) (def ^{:sci.impl/fork-fn host/spy} x 2)"
    "x"]
   ["def, then a root bind"
    "(def ^{:sci.impl/fork-fn host/spy} x 1) (alter-var-root #'x inc)"
    "x"]
   ["defonce"
    "(defonce ^{:sci.impl/fork-fn host/spy} x 2)"
    "x"]
   ["intern with a with-meta symbol"
    "(intern *ns* (with-meta 'x {:sci.impl/fork-fn host/spy}) 2)"
    "x"]
   ["intern with a vary-meta symbol"
    "(intern *ns* (vary-meta 'x assoc :sci.impl/fork-fn host/spy) 2)"
    "x"]
   ["intern unbound (unbind), then a root bind"
    "(intern *ns* (with-meta 'x {:sci.impl/fork-fn host/spy}))
     (alter-var-root (resolve 'x) (constantly 2))"
    "x"]
   ["reset-meta!, then a root bind"
    "(def x 1) (reset-meta! #'x {:sci.impl/fork-fn host/spy}) (alter-var-root #'x inc)"
    "x"]
   ["reset-meta! of an unregistered Var (allocation), then a root bind"
    "(let [v (first host/boxed)]
       (reset-meta! v {:sci.impl/fork-fn host/spy})
       (alter-var-root v inc))"
    "@(first host/boxed)"]
   ["alter-meta!, then a root bind (descriptor upgrade)"
    "(def x 1) (alter-meta! #'x assoc :sci.impl/fork-fn host/spy) (alter-var-root #'x inc)"
    "x"]
   ["alter-meta! of an unregistered Var, then a watch"
    "(let [v (first host/boxed)]
       (alter-meta! v assoc :sci.impl/fork-fn host/spy)
       (add-watch v :k (fn [& _]))
       (alter-var-root v inc))"
    "@(first host/boxed)"]
   ["alter-meta! of an unregistered Var, then remove-watch"
    "(let [v (first host/boxed)]
       (alter-meta! v assoc :sci.impl/fork-fn host/spy)
       (remove-watch v :k)
       (alter-var-root v inc))"
    "@(first host/boxed)"]])

(deftest var-metadata-never-installs-a-copier
  (doseq [[label setup read] metadata-routes]
    (each-route label
      #(let [{:keys [calls ctx]} (spy-env)]
        (sci/eval-string* ctx setup)
        (let [child (sci/fork ctx)]
          (is (zero? @calls) "no copier from metadata ran during the fork")
          (is (= 2 (sci/eval-string* child read)) "the child's value is the parent's")
          (is (= 2 (sci/eval-string* ctx read)) "the parent is unchanged")
          (sci/fork child)
          (is (zero? @calls) "nor in a fork of the child"))))))

(deftest alter-meta-then-unbind-installs-no-copier
  ;; Interpreted code reaches unbind only through intern (above); the host
  ;; path through the same Var operation must not read the copier either.
  (let [{:keys [calls ctx boxed-var]} (spy-env)]
    (sci/eval-string* ctx "(alter-meta! (first host/boxed) assoc :sci.impl/fork-fn host/spy)")
    (sci/call-with-context ctx #(vars/unbind boxed-var))
    (sci/fork ctx)
    (is (zero? @calls))))

(deftest a-host-copier-comes-only-from-fork-fns
  (let [calls (atom [])
        copier (fn [v] (swap! calls conj v) [:copied v])
        v (sci/new-var 'v :value {:ns (sci/create-ns 'host)})
        w (sci/new-var 'w :other {:ns (sci/create-ns 'host)})
        ctx (sci/init {:runtime-mode :forkable
                       :namespaces {'host {'v v 'w w}}
                       :fork-fns {v copier}})]
    (is (= [:copied :value] (sci/eval-string* (sci/fork ctx) "host/v")))
    (is (= [:value] @calls) "the copier ran once, for its Var")
    (testing "installing the same copier again is a no-op"
      (sci/merge-opts ctx {:fork-fns {v copier}})
      (is (= [:copied :value] (sci/eval-string* (sci/fork ctx) "host/v"))))
    (testing "an installed copier never changes"
      (is (thrown-with-msg? #?(:clj Exception :cljs :default) #"different fork fn"
                            (sci/merge-opts ctx {:fork-fns {v identity}}))))
    (testing "the host may install one for a registered Var without one"
      (sci/merge-opts ctx {:fork-fns {w copier}})
      (is (= [:copied :other] (sci/eval-string* (sci/fork ctx) "host/w"))))
    (testing "interpreted metadata cannot replace it"
      (sci/eval-string* ctx "(alter-meta! #'host/v assoc :sci.impl/fork-fn identity)")
      (is (= [:copied :value] (sci/eval-string* (sci/fork ctx) "host/v"))))
    (is (thrown-with-msg? #?(:clj Exception :cljs :default) #"SCI Vars"
                          (sci/init {:runtime-mode :forkable :fork-fns {:not-a-var identity}})))))

;; ---------------------------------------------------------------------------
;; :sci/built-in

(defn- token-env
  "A host token a fork's `:fork-fn` replaces with ::forked: a Var whose value
   escapes fork processing (built-in) keeps the token in the child."
  []
  (let [token (atom :token)]
    {:token token
     :ctx (sci/init {:runtime-mode :forkable
                     :namespaces {'host {'token token}}
                     :fork-fn (fn [v] (if (identical? v token) ::forked v))})}))

(def ^:private set-built-in-routes
  [["def" "(def ^:sci/built-in x host/token)"]
   ["def with an attribute map" "(def ^{:sci/built-in true} x host/token)"]
   ["defonce" "(defonce ^:sci/built-in x host/token)"]
   ["intern with a with-meta symbol" "(intern *ns* (with-meta 'x {:sci/built-in true}) host/token)"]
   ["intern with a vary-meta symbol" "(intern *ns* (vary-meta 'x assoc :sci/built-in true) host/token)"]
   ["intern unbound, then a root bind"
    "(intern *ns* (with-meta 'x {:sci/built-in true})) (alter-var-root (resolve 'x) (constantly host/token))"]
   ["reset-meta!" "(def x host/token) (reset-meta! #'x {:sci/built-in true})"]
   ["alter-meta!" "(def x host/token) (alter-meta! #'x assoc :sci/built-in true)"]])

(deftest interpreted-metadata-cannot-set-built-in-on-a-var
  (doseq [[label setup] set-built-in-routes]
    (each-route label
      #(let [{:keys [ctx]} (token-env)]
        (sci/eval-string* ctx setup)
        (let [child (sci/fork ctx)]
          (is (= ::forked (sci/eval-string* child "x"))
              "the fork processed the value: the Var did not opt out")
          (is (= :token (sci/eval-string* ctx "@x")))
          (doseq [c [ctx child]]
            (is (= 3 (sci/eval-string* c "(alter-var-root (resolve 'x) (constantly 3))"))
                "the Var stays writable")
            (is (= 4 (sci/eval-string* c "(def x 4) x")) "and redefinable")))))))

(deftest interpreted-metadata-cannot-set-built-in-on-a-namespace
  (doseq [[label setup] [["ns attribute map" "(ns app {:sci/built-in true})"]
                         ["ns name metadata" "(ns ^:sci/built-in app)"]
                         ["alter-meta! of *ns*" "(ns app) (alter-meta! *ns* assoc :sci/built-in true)"]
                         ["reset-meta! of *ns*" "(ns app) (reset-meta! *ns* {:sci/built-in true})"]]]
    (each-route label
      #(let [{:keys [ctx]} (token-env)]
        (sci/eval-string* ctx setup)
        (doseq [c [ctx (sci/fork ctx)]]
          (is (= 1 (sci/eval-string* c "(in-ns 'app) (def added 1) added"))
              "the namespace stays writable")
          (is (= 2 (sci/eval-string* c "(clojure.core/intern 'app 'interned 2) app/interned")))
          (is (nil? (sci/eval-string* c "(clojure.core/ns-unmap 'app 'added)"))))))))

(defn- built-in-env []
  (let [host-ns (sci/create-ns 'host.lib {:sci/built-in true})
        token (atom :token)]
    {:token token
     :ctx (sci/init {:runtime-mode :forkable
                     :namespaces {'host.lib {:obj host-ns
                                             'v (sci/new-var 'v token {:ns host-ns :sci/built-in true})}
                                  'host.open {'w (sci/new-var 'w token {:ns (sci/create-ns 'host.open)
                                                                         :sci/built-in true})}}
                     :fork-fn (fn [v] (if (identical? v token) ::forked v))})}))

(deftest interpreted-metadata-cannot-clear-built-in
  (doseq [[label s] [["alter-meta! of a Var" "(alter-meta! #'host.open/w dissoc :sci/built-in)"]
                     ["reset-meta! of a Var" "(reset-meta! #'host.open/w {})"]
                     ["def over it" "(in-ns 'host.open) (def ^{:sci/built-in false} w 1)"]
                     ["intern over it" "(intern 'host.open (with-meta 'w {:sci/built-in false}) 1)"]
                     ["alter-meta! of a namespace" "(alter-meta! (find-ns 'host.lib) dissoc :sci/built-in)"]
                     ["reset-meta! of a namespace" "(reset-meta! (find-ns 'host.lib) {})"]
                     ;; CLJS: the ns form writes the namespace's metadata
                     ;; without the write check, so only the flag protects it
                     ["ns over it" "(ns host.lib {:sci/built-in false})"]]]
    (each-route label
      #(let [{:keys [ctx token]} (built-in-env)]
         (refused ctx s)
         (doseq [c [ctx (sci/fork ctx)]]
           (is (re-find #"read-only" (str (refused c "(alter-var-root #'host.open/w (constantly 1))"))))
           (is (re-find #"read-only" (str (refused c "(in-ns 'host.lib) (def added 1)"))))
           (is (identical? token (sci/eval-string* c "host.open/w"))
               "a built-in Var's value is not processed by forks"))))))

(deftest an-unrestricted-evaluation-may-set-but-not-clear
  ;; e.g. spindel marks Vars it loads from source read-only this way
  (let [{:keys [ctx]} (token-env)
        unrestricted (assoc ctx :unrestricted true)]
    (sci/eval-string* ctx "(def x 1) (ns app) (in-ns 'user)")
    (sci/eval-string* unrestricted "(alter-meta! #'x assoc :sci/built-in true)
                                    (alter-meta! (find-ns 'app) assoc :sci/built-in true)")
    (is (re-find #"read-only" (str (refused ctx "(alter-var-root #'x inc)"))))
    (is (re-find #"read-only" (str (refused ctx "(in-ns 'app) (def y 1)"))))
    (is (true? (sci/eval-string* ctx "(:sci/built-in (meta #'x))")))
    (testing "the flag is shared by every context: an unrestricted evaluation cannot clear it"
      (sci/eval-string* unrestricted "(alter-meta! #'x dissoc :sci/built-in) (reset-meta! #'x {})")
      (is (re-find #"read-only" (str (refused ctx "(alter-var-root #'x inc)"))))
      (is (true? (sci/eval-string* ctx "(:sci/built-in (meta #'x))"))))))

(deftest an-unrestricted-context-cannot-unprotect-shared-vars-for-others
  (doseq [mode [:standard :forkable]]
    (each-route (name mode)
      #(let [u (assoc (sci/init {:runtime-mode mode}) :unrestricted true)
             other (sci/init {:runtime-mode mode})]
         (sci/eval-string* u "(reset-meta! #'clojure.core/frequencies {:doc \"x\"})
                              (alter-meta! #'clojure.core/frequencies dissoc :sci/built-in)")
         (is (re-find #"read-only"
                      (str (refused other "(alter-var-root #'clojure.core/frequencies (constantly (fn [_] :pwned)))"))))
         (is (= {1 2} (sci/eval-string* (sci/init {}) "(frequencies [1 1])")))))))

(deftest a-fork-with-older-metadata-reports-and-keeps-the-flag
  (let [{:keys [ctx]} (token-env)
        _ (sci/eval-string* ctx "(def x 1)")
        before (sci/fork ctx)]
    (sci/eval-string* (assoc ctx :unrestricted true) "(alter-meta! #'x assoc :sci/built-in true)")
    (is (true? (sci/eval-string* before "(:sci/built-in (meta #'x))"))
        "a read in a fork made before the flag reports it")
    (sci/eval-string* (assoc before :unrestricted true) "(alter-meta! #'x assoc :doc \"d\")")
    (doseq [c [ctx before]]
      (is (re-find #"read-only" (str (refused c "(alter-var-root #'x inc)")))))))

;; ---------------------------------------------------------------------------
;; Host code is trusted provenance

(deftest host-metadata-writes-set-the-flag
  (doseq [mode [:standard :forkable]
          [label mark!] (cond->
                            [["sci/alter-var-meta!" #(sci/alter-var-meta! % assoc :sci/built-in true)]
                             ["sci/reset-var-meta!" #(sci/reset-var-meta! % (assoc (meta %) :sci/built-in true))]]
                            ;; CLJS's alter-meta! writes the object field directly
                            #?(:clj true :cljs false)
                            (conj ["host alter-meta!" #(alter-meta! % assoc :sci/built-in true)]))]
    (each-route (str (name mode) " " label)
      #(let [v (sci/new-var 'h 1 {:ns (sci/create-ns 'host.h)})
             ctx (sci/init {:runtime-mode mode :namespaces {'host.h {'h v}}})]
         (mark! v)
         (is (true? (:sci/built-in (meta v))))
         (is (true? (sci/eval-string* ctx "(:sci/built-in (meta #'host.h/h))")))
         (is (re-find #"read-only" (str (refused ctx "(alter-var-root #'host.h/h inc)"))))
         (is (= 1 (sci/eval-string* ctx "host.h/h")))))))

(deftest metadata-reports-the-flag
  (let [{:keys [ctx]} (token-env)]
    (sci/eval-string* ctx "(def x 1) (alter-meta! #'x assoc :sci/built-in true)
                           (def ^:sci/built-in y 1)")
    (is (= [nil nil] (sci/eval-string* ctx "[(:sci/built-in (meta #'x)) (:sci/built-in (meta #'y))]")))
    (is (true? (sci/eval-string* ctx "(:sci/built-in (meta #'clojure.core/inc))")))))

;; ---------------------------------------------------------------------------
;; Interop cannot reach SCI's internal members

#?(:clj
   (def ^:private interop-routes
     ["(._set_built_in_BANG_ #'clojure.core/frequencies false)"
      "(._set_built_in_BANG_ ^sci.lang.Var #'clojure.core/frequencies false)"
      "(sci.lang.Var/._set_built_in_BANG_ #'clojure.core/frequencies false)"
      "(._mirror_root_BANG_ #'clojure.core/frequencies (fn [_] :pwned))"
      "(._mirror_meta_BANG_ #'clojure.core/frequencies {})"
      "(._mark_world_tracked_BANG_ #'clojure.core/frequencies {})"
      "(._world_home_ctx #'clojure.core/frequencies)"
      "(.setThreadBound #'clojure.core/frequencies true)"
      "(.bindRoot #'clojure.core/frequencies (fn [_] :pwned))"
      "(.getRawRoot #'clojure.core/frequencies)"
      "(.-sym #'clojure.core/frequencies)"
      "(sci.lang.Var. (fn [_] :pwned) 'f {} false false nil *ns* {:unrestricted true} false)"
      "(sci.lang.Var/new (fn [_] :pwned) 'f {} false false nil *ns* {:unrestricted true} false)"
      "(sci.lang.Var/getBasis)"
      "(._set_built_in_BANG_ *ns* false)"
      "(._mirror_meta_BANG_ *ns* {})"
      "(do (deftype T []) (.setVal T {}))"
      "(do (deftype T []) (._mark_world_tracked_BANG_ T nil))"
      "(do (deftype T []) (._mirror_root_BANG_ T {}))"
      "(sci.lang.Type. {} (atom nil))"])
   :cljs
   (def ^:private interop-routes
     ["(.sci$impl$world$IBuiltIn$_set_built_in_BANG_$arity$2 #'clojure.core/frequencies #'clojure.core/frequencies false)"
      "(.sci$lang$IWorldMirror$_mirror_root_BANG_$arity$2 #'clojure.core/frequencies #'clojure.core/frequencies (fn [_] :pwned))"
      "(.-built_in #'clojure.core/frequencies)"
      "(.-world_tracked #'clojure.core/frequencies)"
      "(set! (.-built_in #'clojure.core/frequencies) false)"
      "(set! (.-root #'clojure.core/frequencies) (fn [_] :pwned))"
      "(do (deftype T []) (set! (.-data T) {}))"
      "(do (deftype T []) (.-data T))"
      "(sci.lang.Type. {} (atom nil))"
      "(.-prototype sci.lang.Type)"]))

(deftest interop-cannot-reach-internal-members
  (doseq [mode [:standard :forkable]
          s interop-routes]
    (each-route (str (name mode) " " s)
      #(let [ctx (sci/init {:runtime-mode mode})]
         (is (some? (refused ctx s)) "refused")
         (is (re-find #"read-only"
                      (str (refused ctx "(alter-var-root #'clojure.core/frequencies (constantly (fn [_] :pwned)))"))))
         (doseq [c [ctx (sci/init {}) (sci/init {:runtime-mode :forkable})]]
           (is (= {1 2} (sci/eval-string* c "(frequencies [1 1])"))))))))

(deftest interop-on-internal-objects-keeps-object-methods
  (let [ctx (sci/init {:runtime-mode :forkable})]
    #?(:clj (is (= "#'clojure.core/inc" (sci/eval-string* ctx "(.toString #'clojure.core/inc)"))))
    #?(:clj (is (true? (sci/eval-string* ctx "(instance? sci.lang.Var #'inc)"))))))

;; ---------------------------------------------------------------------------
;; dvergr's clojure.test adapter: host Vars whose identity must survive forks

(deftest host-vars-with-fork-policy-through-merge-opts
  (let [tns (sci/create-ns 'host.test nil)
        out (sci/new-dynamic-var '*test-out* (atom []) {:ns tns})
        counters (sci/new-dynamic-var '*report-counters* nil {:ns tns})
        ;; the implementation reads the same Var handles the interpreted
        ;; code rebinds
        report (sci/new-var 'report
                            (fn [m]
                              (some-> @counters (swap! update (:type m) (fnil inc 0)))
                              (swap! @out conj m)
                              nil)
                            {:ns tns})
        ctx (sci/init {:runtime-mode :forkable})]
    (sci/merge-opts ctx {:namespaces {'host.test {:obj tns
                                                  '*test-out* out
                                                  '*report-counters* counters
                                                  'report report}}
                         :fork-fns {out identity report identity}})
    (let [child (sci/fork ctx)
          grandchild (sci/fork child)]
      (doseq [[label c] [["parent" ctx] ["child" child] ["grandchild" grandchild]]]
        (testing label
          (is (= [{:type :pass} {:type :fail}]
                 (sci/eval-string*
                  c "(require '[host.test :as t])
                     (let [sink (atom [])]
                       (binding [t/*test-out* sink
                                 t/*report-counters* (atom {})]
                         (t/report {:type :pass})
                         (t/report {:type :fail})
                         @sink))"))
              "the host function sees the child's dynamic bindings of the same Vars")
          (is (identical? out (sci/eval-string* c "#'host.test/*test-out*"))))))))

;; ---------------------------------------------------------------------------
;; CLJS: the JS prototype the fixed type copier consumes

#?(:cljs
   (defn- trapping-prototype
     "A host JS prototype whose traps record a call: run during a fork, they
      would be user code inside the copy."
     [calls]
     (js/Proxy. #js {}
                #js {:getPrototypeOf (fn [_] (swap! calls inc)
                                       (.-prototype sci.impl.deftype/SciType))
                     :ownKeys (fn [_] (swap! calls inc) #js [])})))

#?(:cljs
   (deftest type-metadata-cannot-replace-the-prototype-operand
     (doseq [[label s] [["alter-meta!" "(alter-meta! T assoc :sci.impl/js-prototype host/proto)"]
                        ["-create-type" "(sci.impl.deftype/-create-type
                                          {:sci.impl/type-name 'user.T
                                           :sci.impl/js-prototype host/proto}
                                          T 'user)"]]]
       (each-route label
         #(let [calls (atom 0)
               proto (trapping-prototype calls)
               ctx (sci/init {:runtime-mode :forkable
                              :namespaces {'host {'proto proto}}})]
           (sci/eval-string* ctx "(deftype T [a]) (def original (:sci.impl/js-prototype (meta T)))")
           (sci/eval-string* ctx s)
           (is (sci/eval-string* ctx "(identical? original (:sci.impl/js-prototype (meta T)))")
               "the type keeps its prototype")
           (reset! calls 0)
           (let [child (sci/fork ctx)]
             (is (zero? @calls) "the copier did not touch the supplied object")
             (is (= 1 (sci/eval-string* child "(.-a (->T 1))")))
             (is (not (sci/eval-string* child "(identical? original (:sci.impl/js-prototype (meta T)))"))
                 "the child has its own copy of the prototype")))))))
