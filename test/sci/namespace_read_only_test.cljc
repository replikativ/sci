(ns sci.namespace-read-only-test
  "A host namespace marked `:sci/built-in` keeps its mappings: interpreted
   code cannot add, remove or reload names in it."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]))

(defn- ctx
  ([] (ctx {}))
  ([opts]
   (sci/init (merge {:namespaces {'host.lib {:obj (sci/create-ns 'host.lib {:sci/built-in true})
                                             'f identity}
                                  'host.open {'g identity}}
                     :load-fn (fn [{:keys [namespace]}]
                                {:file (str namespace ".clj")
                                 :source (str "(ns " namespace ") (def reloaded 1)")})}
                    opts))))

(defn- refused [c s]
  (try (sci/eval-string* c s) nil
       (catch #?(:clj Exception :cljs :default) e (ex-message e))))

(def ^:private read-only #"[Nn]amespace host.lib is read-only")

(deftest a-read-only-namespace-refuses-new-names
  (doseq [mode [:standard :forkable]]
    (testing (name mode)
      (let [c (ctx {:runtime-mode mode})]
        (is (re-find read-only (str (refused c "(in-ns 'host.lib) (def added 1)"))) "def")
        (is (re-find read-only (str (refused c "(in-ns 'host.lib) (defn added [] 1)"))) "defn")
        (is (re-find read-only (str (refused c "(in-ns 'host.lib) (declare added)"))) "declare")
        (is (re-find read-only (str (refused c "(in-ns 'host.lib) (deftype Added [])"))) "deftype")
        (is (re-find read-only (str (refused c "(intern 'host.lib 'added 1)"))) "intern")
        (is (re-find read-only (str (refused c "(intern 'host.lib 'added)"))) "intern unbound")
        (is (re-find read-only (str (refused c "(ns-unmap 'host.lib 'f)"))) "ns-unmap")
        (is (re-find read-only (str (refused c "(remove-ns 'host.lib)"))) "remove-ns")
        (is (re-find read-only (str (refused c "(require 'host.lib :reload)"))) "require :reload")
        (is (re-find read-only (str (refused c "(require 'host.lib :reload-all)"))) "require :reload-all")
        (is (= [false false 2]
               (sci/eval-string* c "[(contains? (ns-interns 'host.lib) 'added)
                                      (contains? (ns-publics 'host.lib) 'reloaded)
                                      ((ns-resolve 'host.lib 'f) 2)]"))
            "the namespace is unchanged")
        (is (= 3 (sci/eval-string* c "(require '[host.lib :as l]) (l/f 3)"))
            "using and requiring it still works")))))

(deftest other-namespaces-are-unaffected
  (let [c (ctx)]
    (is (= [1 2 3 nil true]
           (sci/eval-string*
            c "(def a 1)
               (in-ns 'host.open) (def b 2)
               (clojure.core/intern 'host.open 'c 3)
               (clojure.core/ns-unmap 'host.open 'g)
               (in-ns 'user)
               [a host.open/b host.open/c (ns-resolve 'host.open 'g)
                (do (remove-ns 'host.open) (nil? (find-ns 'host.open)))]")))
    (is (= 1 (sci/eval-string* c "(require 'other.lib :reload) other.lib/reloaded")))))

(deftest unrestricted-contexts-and-the-host-may-extend-it
  (is (= 1 (sci/eval-string* (ctx {:unrestricted true}) "(in-ns 'host.lib) (def added 1) added")))
  (let [c (ctx)]
    (sci/add-namespace! c 'host.lib {'h inc})
    (is (= 2 (sci/eval-string* c "(host.lib/h 1)")))))

(deftest deny-covers-forked-worlds-and-read-only-namespaces
  ;; :deny on namespace introspection and aliasing (babashka/sci#1134) holds
  ;; in a forked world, for a read-only host namespace and for a namespace
  ;; that exists only in the child's world.
  ;; `f` is a Var: :deny reaches an aliased name through the Var's qualified
  ;; name (a raw value under an alias is not covered, as in :standard mode).
  (let [host-ns (sci/create-ns 'host.lib {:sci/built-in true})
        parent (sci/init {:runtime-mode :forkable
                          :namespaces {'host.lib {:obj host-ns
                                                  'f (sci/new-var 'f identity {:ns host-ns})}}
                          :deny '[host.lib/f inc app/secret kid/secret]})
        _ (sci/eval-string* parent "(ns app) (def secret 1) (def visible 2) (in-ns 'user)")
        child (sci/fork parent)
        _ (sci/eval-string* child "(ns kid) (def secret 3) (def visible 4) (in-ns 'user)")]
    (doseq [[label c] [["parent" parent] ["child" child]]]
      (testing label
        (is (= [false false false false nil nil false true]
               (sci/eval-string*
                c "[(contains? (ns-publics 'host.lib) 'f)
                    (contains? (ns-interns 'host.lib) 'f)
                    (contains? (ns-refers *ns*) 'inc)
                    (contains? (ns-map *ns*) 'inc)
                    (find-var 'host.lib/f)
                    (find-var 'clojure.core/inc)
                    (contains? (ns-publics 'app) 'secret)
                    (contains? (ns-publics 'app) 'visible)]")))
        (doseq [expr ["(do (alias 'h 'host.lib) (h/f 1))"
                      "(do (require '[host.lib :as h]) (h/f 1))"
                      "(do (refer 'host.lib :rename '{f g}) (g 1))"
                      "(@(intern 'host.lib 'f) 1)"
                      "(do (alias 'c 'clojure.core) (c/inc 1))"
                      "(@(intern 'app 'secret) 1)"]]
          (is (re-find #"not allowed" (str (refused c expr))) expr))))
    (testing "a namespace that exists only in the child"
      (is (= [false true nil]
             (sci/eval-string*
              child "[(contains? (ns-publics 'kid) 'secret)
                      (contains? (ns-interns 'kid) 'visible)
                      (find-var 'kid/secret)]")))
      (is (re-find #"not allowed" (str (refused child "kid/secret"))))
      (is (nil? (sci/eval-string* parent "(find-ns 'kid)"))))))
