(ns rt-in-weekend.bezier.complex-object
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.bezier.patch :as bezier-spline]
            [com.climate.claypoole :as cp]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [rt-in-weekend.util :as ut]
            [clj-tuple :as ct]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;;;

(defn scale-radius [^double radius spatial-tree-root-node]
  (bezier-spline/transform-node :radius
                                (fn [^double value] (* value radius))
                                spatial-tree-root-node))

(defn scale-center [center radius spatial-tree-root-node]
  (bezier-spline/transform-node :center
                                (fn [value] (-> value
                                                (v/mult radius)
                                                (v/add center)))
                                spatial-tree-root-node))

(defn unrolled-tree [node]
  (->> (dissoc node :children)
       (into [])
       (cons [:children (some->> (:children node)
                                 (apply ct/vector))])
       (filter (comp some? second))
       (apply concat)
       (apply ct/hash-map)))

(defrecord Surfaces [threadpool normalized-trees-forest epsil iteration-limits-sample-sizes center radius material]
  BuildableProto
  (build [_]
    (cp/pmap threadpool
             (fn [[control-points spatial-tree-root-node]]
               (bezier-spline/->Surface (->> spatial-tree-root-node
                                             (into {})
                                             (scale-radius radius)
                                             (scale-center center radius)
                                             (bezier-spline/add-spheres)
                                             (unrolled-tree))
                                        (mapv (fn [value] (-> value
                                                              (v/mult radius)
                                                              (v/add center)))
                                              control-points)
                                        epsil iteration-limits-sample-sizes material))
             normalized-trees-forest)))

(defrecord Body [bounding-sphere surfaces]
  HitableProto
  (hit [_ ray t-min t-max]
    (when (some? (hit bounding-sphere ray t-min t-max))
      (some->> surfaces
               (map (fn [surface]
                      (hit surface ray t-min t-max)))
               (filter some?)
               (seq)
               (min-hit)))))

;;;

(defn create [threadpool normalized-trees-forest epsil iteration-limits-sample-sizes center radius material]
  (let [surfaces        (-> (->Surfaces threadpool normalized-trees-forest epsil iteration-limits-sample-sizes center radius material)
                            (build))
        Center          (->> surfaces
                             (map (comp :center :spatial-tree-root-node))
                             (reduce v/add)
                             (ut/flip v/div (count surfaces)))
        R               (->> surfaces
                             (sort-by (comp :radius :spatial-tree-root-node))
                             (last)
                             :spatial-tree-root-node
                             :radius)
        r               (->> surfaces
                             (map (comp (partial v/dist Center)
                                        (comp :center :spatial-tree-root-node)))
                             (reduce m/fast-max))
        bounding-sphere (->Sphere Center (+ ^double R ^double r) nil)]
    (->Body bounding-sphere surfaces)))
