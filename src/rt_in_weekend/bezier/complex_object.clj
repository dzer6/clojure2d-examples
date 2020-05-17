(ns rt-in-weekend.bezier.complex-object
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.bezier.patch :as bezier-spline]
            [com.climate.claypoole :as cp]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [rt-in-weekend.util :as ut]))

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

(defrecord Surfaces [threadpool normalized-trees-forest epsil iteration-limit sample-size center radius material]
  BuildableProto
  (build [_]
    (->> normalized-trees-forest
         (mapv (fn [[control-points spatial-tree-root-node]]
                 [(->> control-points
                       (mapv (fn [value] (-> value
                                             (v/mult radius)
                                             (v/add center)))))
                  (->> spatial-tree-root-node
                       (scale-radius radius)
                       (scale-center center radius)
                       (bezier-spline/add-spheres))]))
         (cp/pmap threadpool (fn [[control-points tree-node]]
                               (bezier-spline/->Surface tree-node
                                                        control-points
                                                        epsil iteration-limit sample-size material))))))

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

(defn create [threadpool normalized-trees-forest epsil iteration-limit sample-size center radius material]
  (let [surfaces (-> (->Surfaces threadpool normalized-trees-forest epsil iteration-limit sample-size center radius material)
                     (build))
        Center (->> surfaces
                    (map (comp :center :spatial-tree-root-node))
                    (reduce v/add)
                    (ut/flip v/div (count surfaces)))
        R (->> surfaces
               (sort-by (comp :radius :spatial-tree-root-node))
               (last)
               :spatial-tree-root-node
               :radius)
        r (->> surfaces
               (map (comp (partial v/dist Center)
                          (comp :center :spatial-tree-root-node)))
               (reduce m/fast-max))
        bounding-sphere (->Sphere Center (+ ^double R ^double r) nil)]
    (->Body bounding-sphere surfaces)))
