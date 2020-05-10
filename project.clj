(defproject clojure2d-examples "1.3.0"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url  "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.memoize "1.0.236"]
                 [com.climate/claypoole "1.1.4"]
                 [uncomplicate/neanderthal "0.31.0"]
                 [criterium "0.4.5"]
                 [prismatic/hiphip "0.2.1"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [clatrix/clatrix "0.5.0"]
                 [org.nd4j/nd4j-native-platform "1.0.0-beta"]
                 [org.nd4j/nd4j-api "1.0.0-beta"]
                 [net.littleredcomputer/sicmutils "0.12.0"]
                 [generateme/fastmath "1.5.2"]
                 [clojure2d "1.3.1"]]

  :plugins [[lein-ancient "0.6.15"]]

  :repl-options {:timeout 120000}
  :target-path "target/%s"

  :jvm-opts ^:replace ["-Dserver -Dclojure.compiler.direct-linking=true"
                       "-Dclojure.compiler.direct-linking=true"
                       "-Djava.library.path=$LD_LIBRARY_PATH"
                       "-Dorg.bytedeco.javacpp.openblas.load=mkl_rt"]
  )
