(ns rt-in-weekend.bezier.patch-file-format
  (:require [rt-in-weekend.util :as ut]
            [clojure.string :as string]
            [fastmath.vector :as v]))

;; http://www.holmes3d.net/graphics/roffview/tools/patchoff/
(defn load-control-points [path]
  (->> (slurp path)
       (ut/flip string/split #"3 3\r")
       (mapv string/trim)
       (mapv string/trim-newline)
       (rest)
       (mapv (partial ut/flip string/replace "" #"\r"))
       (mapv (partial ut/flip string/split #"\s+"))
       (mapv (partial mapv clojure.edn/read-string))
       (mapv (partial partition 3))
       (mapv (partial mapv (partial apply v/vec3)))))
