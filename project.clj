(defproject org.replikativ/sci
  #=(clojure.string/trim
     #=(slurp "resources/SCI_VERSION"))
  ;; :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
  :description "Replikativ compatibility distribution of SCI with forkable interpreter worlds"
  :url "https://github.com/replikativ/sci"
  :scm {:name "git"
        :url "https://github.com/replikativ/sci"
        :connection "scm:git:https://github.com/replikativ/sci.git"
        :developerConnection "scm:git:ssh://git@github.com/replikativ/sci.git"
        :tag "replikativ-v0.15.59.1"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [borkdude/edamame "1.6.43"]
                 [org.babashka/sci.impl.types "0.0.3"]
                 [borkdude/graal.locking "0.0.2"]]
  :plugins [[lein-codox "0.10.7"]]
  :profiles {:clojure-1.10.3 {:depdencies [[org.clojure/clojure "1.10.3"]]}
             :clojure-1.11.1 {:dependencies [[org.clojure/clojure "1.11.1"]]}
             :native-image {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :dev {:source-paths ["dev"]
                   :dependencies [[thheller/shadow-cljs "2.8.64"]]}
             :test {:resource-paths ["test-resources"]
                    :jvm-opts ["-Djdk.attach.allowAttachSelf"]
                    :dependencies [[clj-commons/conch "0.9.2"]
                                   [criterium "0.4.5"]
                                   [com.clojure-goes-fast/clj-async-profiler "0.4.0"]]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :aot [sci.impl.main]
                       :main sci.impl.main}
             :libsci {:dependencies [[cheshire "5.10.0"]]
                      :source-paths ["src" "libsci/src"]
                      :aot [sci.impl.libsci]}}
  ;; for testing only
  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]])

;; Notes
;; Generate a bundle size report with shadow-cljs:
;; npx shadow-cljs run shadow.cljs.build-report sci report.html
