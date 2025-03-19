(ns rt-in-weekend.bezier.normalized-trees-forest
  (:require [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.bezier.patch :as bezier-spline]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [com.climate.claypoole :as cp]
            [rt-in-weekend.bezier.patch-file-format :as bpt]
            [clj-tuple :as ct])
  (:import (fastmath.vector Vec3)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;;;

(defn create-control-points-normalized-spatial-tree-tuple [^Vec3 center
                                                           ^double radius
                                                           tree-levels-number
                                                           control-points]
  (let [normalized-control-points (mapv (fn [^Vec3 control-point]
                                          (-> (v/sub control-point center)
                                              (v/div radius)))
                                        control-points)
        tree                      (->> normalized-control-points
                                       (bezier-spline/->BezierSpatialTree tree-levels-number)
                                       (build))]
    (ct/vector normalized-control-points tree)))

(defn object-normalized-spatial-trees [control-points-vec threadpool tree-levels-number]
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

(defrecord NormalizedTreesForest [threadpool tree-levels-number transformation-fn path]
  BuildableProto
  (build [_]
    (-> (bpt/load-control-points path)
        (cond->> transformation-fn (mapv (partial mapv transformation-fn)))
        (object-normalized-spatial-trees threadpool tree-levels-number))))

;;;

(defn create [threadpool tree-levels-number transformation-fn path]
  (->> path
       (->NormalizedTreesForest threadpool tree-levels-number transformation-fn)
       (build)))