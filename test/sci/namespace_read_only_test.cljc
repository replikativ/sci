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
