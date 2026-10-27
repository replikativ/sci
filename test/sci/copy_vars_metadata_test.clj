(ns sci.copy-vars-metadata-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.impl.copy-vars :as copy-vars]))

(defn- copied-metadata [resolved-name input-symbol]
  (with-redefs [copy-vars/cljs-resolve
                (fn [_env _sym] {:name resolved-name :meta {:macro true}})]
    (copy-vars/var-meta {:ns {:name 'metadata-test}} input-symbol {:macro true})))

(deftest self-hosted-native-core-macros-retain-their-analyzer-fast-path
  (doseq [core-ns [nil "clojure.core" "cljs.core" "cljd.core" "cljs.core$macros"]
          macro-name '[lazy-seq or and ns]]
    (testing (str core-ns "/" macro-name)
      (is (= (list 'quote macro-name)
             (:sci.impl/fast-path
              (copied-metadata (symbol core-ns (name macro-name)) macro-name)))))))

(deftest foreign-macro-names-do-not-inherit-core-fast-path-or-inline-capabilities
  (doseq [foreign-ns ["business.tools" "business.tools$macros"
                     "business.cljs.core$macros" "cljs.core$macros-extra"]]
    (testing foreign-ns
      (doseq [macro-name '[lazy-seq or and ns]]
        (is (not (contains? (copied-metadata (symbol foreign-ns (name macro-name)) macro-name)
                            :sci.impl/fast-path))))
      (is (not (contains? (copied-metadata (symbol foreign-ns "get") 'get)
                          :sci.impl/inlined))))))
