(ns rt-in-weekend.bezier.complex-object
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.bezier.patch :as bezier-spline]
            [com.climate.claypoole :as cp]
            [fastmath.vector :as v]))

;;;

(defn scale-radius [radius spatial-tree-root-node]
  (bezier-spline/transform-node :radius
                                (fn [value] (* value radius))
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

(defrecord Body [surfaces]
  HitableProto
  (hit [_ ray t-min t-max]
    (some->> surfaces
             (map (fn [surface]
                    (hit surface ray t-min t-max)))
             (filter some?)
             (seq)
             (min-hit))))

;;;

(defn create [threadpool normalized-trees-forest epsil iteration-limit sample-size center radius material]
  (->> (->Surfaces threadpool normalized-trees-forest epsil iteration-limit sample-size center radius material)
       (.build)
       (->Body)))
