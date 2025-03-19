(defproject clojure2d-examples "1.3.0"
  :description "Examples for Clojure2d library"
  :url "https://github.com/dzer6/clojure2d-examples"
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/core.memoize "1.1.266"]

                 [org.clojure/tools.logging "1.3.0"]
                 [ch.qos.logback/logback-classic "1.5.18"]
                 [ch.qos.logback.contrib/logback-json-classic "0.1.5"]
                 [ch.qos.logback.contrib/logback-jackson "0.1.5"]
                 [org.slf4j/slf4j-api "2.0.17"]
                 [org.slf4j/jul-to-slf4j "2.0.17"]          ; JUL to SLF4J
                 [org.slf4j/jcl-over-slf4j "2.0.17"]        ; JCL to SLF4J
                 [org.slf4j/log4j-over-slf4j "2.0.17"]      ; Log4j to SLF4J

                 [com.climate/claypoole "1.1.4"]
                 [clj-tuple "0.2.2"]
                 [com.rpl/specter "1.1.4"]
                 [criterium "0.4.6"]
                 [net.mikera/core.matrix "0.63.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [generateme/fastmath "1.5.2"]
                 [clojure2d "1.3.1"]]

  :source-paths ["src/main/clojure"]
  :resource-paths ["src/main/resources"]

  :plugins [[lein-ancient "0.7.0"]]

  :repl-options {:timeout 120000}
  :target-path "target/%s")
