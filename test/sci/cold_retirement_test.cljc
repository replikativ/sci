(ns sci.cold-retirement-test
  (:require [sci.core :as sci]
            [sci.impl.namespaces :as namespaces]
            [sci.impl.utils :as utils]
            [sci.impl.world :as world]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest trusted-singleton-imports-do-not-retain-the-first-interpreter
  ;; A fresh native Var makes this independent of suite order: common core
  ;; Vars may already have been imported by several unrelated worlds.
  (let [ns-obj (sci/create-ns 'fixture.lifecycle)
        shared (utils/new-var 'value 42 {:ns ns-obj})]
    (with-redefs [namespaces/namespaces
                  (assoc namespaces/namespaces 'fixture.lifecycle
                         {:obj ns-obj 'value shared})]
      (let [first-root (sci/init {:runtime-mode :forkable})]
        (try
          (is (nil? (world/-world-home-ctx shared)))
          (is (= 42 (sci/eval-string* first-root "fixture.lifecycle/value")))
          (is (nil? (:sci/built-in (sci/eval-string* first-root "(meta #'fixture.lifecycle/value)"))))
          (let [child (sci/fork first-root)
                sibling (sci/fork first-root)]
            (try
              (is (= 43 (sci/eval-string* child "(alter-var-root #'fixture.lifecycle/value inc)")))
              (is (= 42 (sci/eval-string* sibling "fixture.lifecycle/value")))
              (is (= 42 (sci/eval-string* first-root "fixture.lifecycle/value")))
              (finally (sci/retire! child) (sci/retire! sibling))))
          (sci/retire! first-root)
          (let [next-root (sci/init {:runtime-mode :forkable})]
            (try
              (is (= 42 (sci/eval-string* next-root "fixture.lifecycle/value")))
              (is (= 3 (sci/eval-string* next-root "(+ 1 2)")))
              (finally (sci/retire! next-root))))
          (finally (sci/retire! first-root)))))))

(deftest user-vars-from-retired-heaps-do-not-become-trusted-singletons
  (let [first-root (sci/init {:runtime-mode :forkable})
        v (sci/eval-string* first-root "(def value 42) #'value")]
    (sci/retire! first-root)
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error) #"retired"
         (sci/init {:runtime-mode :forkable
                    :namespaces {'clojure.core {'*assert* v}}})))))

(deftest native-handle-aliases-are-home-less-before-any-environment-registration
  (let [ns-obj (sci/create-ns 'fixture.lifecycle)
        shared (utils/new-var 'value 42 {:ns ns-obj})
        alias-ns (sci/create-ns 'fixture.alias)]
    (with-redefs [namespaces/namespaces
                  (assoc namespaces/namespaces 'fixture.lifecycle
                         {:obj ns-obj 'value shared})]
      ;; An environment can visit the alias before the canonical native entry.
      (let [first-root
            (sci/init {:runtime-mode :forkable
                       :env (atom {:namespaces
                                   (array-map 'fixture.alias {:obj alias-ns 'value shared}
                                              'fixture.lifecycle {:obj ns-obj 'value shared}
                                              'clojure.core (get namespaces/namespaces 'clojure.core))})})]
        (try
          (is (nil? (world/-world-home-ctx shared)))
          (sci/retire! first-root)
          (let [next-root (sci/init {:runtime-mode :forkable})]
            (try
              (is (= 42 (sci/eval-string* next-root "fixture.lifecycle/value")))
              (finally (sci/retire! next-root))))
          (finally (sci/retire! first-root)))))))
