(ns rt-in-weekend.bezier.normalized-trees-forest
  (:require [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.bezier.patch :as bezier-spline]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [com.climate.claypoole :as cp]
            [rt-in-weekend.bezier.patch-file-format :as bpt]))

(defn create-control-points-normalized-spatial-tree-tuple [center radius tree-levels-number control-points]
  (let [normalized-control-points (mapv (fn [control-point]
                                          (-> (v/sub control-point center)
                                              (v/div radius)))
                                        control-points)
        tree (->> normalized-control-points
                  (bezier-spline/->BezierSpatialTree tree-levels-number)
                  (.build))]
    [normalized-control-points tree]))

(defn object-normalized-spatial-trees [threadpool tree-levels-number control-points-vec]
  (let [center (->> control-points-vec
                    (reduce into [])
                    ((comp (partial apply v/div)
                           (juxt (partial reduce v/add) count))))
        radius (->> control-points-vec
                    (reduce into [])
                    (map (partial v/dist center))
                    (reduce m/fast-max))]
    (cp/pmap threadpool
             (partial create-control-points-normalized-spatial-tree-tuple center radius tree-levels-number)
             control-points-vec)))

;;;

(defrecord NormalizedTreesForest [threadpool tree-levels-number path]
  BuildableProto
  (build [_]
    (->> (bpt/load-control-points path)
         (object-normalized-spatial-trees threadpool tree-levels-number))))

;;;

(defn create [threadpool tree-levels-number path]
  (->> path
       (->NormalizedTreesForest threadpool tree-levels-number)
       (.build)))