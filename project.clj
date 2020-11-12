(defproject clojure2d-examples "1.3.0"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url  "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.memoize "1.0.236"]

                 [org.clojure/tools.logging "1.0.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [ch.qos.logback.contrib/logback-json-classic "0.1.5"]
                 [ch.qos.logback.contrib/logback-jackson "0.1.5"]
                 [org.slf4j/slf4j-api "1.7.30"]
                 [org.slf4j/jul-to-slf4j "1.7.30"]          ; JUL to SLF4J
                 [org.slf4j/jcl-over-slf4j "1.7.30"]        ; JCL to SLF4J
                 [org.slf4j/log4j-over-slf4j "1.7.30"]      ; Log4j to SLF4J

                 [com.climate/claypoole "1.1.4"]
                 [clj-tuple "0.2.2"]
                 [com.rpl/specter "1.1.3"]
                 [criterium "0.4.5"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [generateme/fastmath "1.5.2"]
                 [clojure2d "1.3.1"]]

  :source-paths ["src/main/clojure"]
  :resource-paths ["src/main/resources"]

  :plugins [[lein-ancient "0.6.15"]]

  :repl-options {:timeout 120000}
  :target-path "target/%s")
