(ns rt-in-weekend.ch10-camera2
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.camera :refer :all]
            [rt-in-weekend.material :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [rt-in-weekend.bezier-spline :as bezier-spline]
            [com.climate.claypoole :as cp])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(def ^:const zero (v/vec3 0.0 0.0 0.0))

(def control-points [(v/vec3 -2 -4 -3)
                     (v/vec3 -2 0 -5)
                     (v/vec3 -2 1 -5)
                     (v/vec3 -2 3 -3)

                     (v/vec3 0 -1 -3)
                     (v/vec3 0 0 -4)
                     (v/vec3 0 1 -9)
                     (v/vec3 0 3 -3)

                     (v/vec3 1 -1 -3)
                     (v/vec3 1 0 -9)
                     (v/vec3 1 1 -4)
                     (v/vec3 1 3 -3)

                     (v/vec3 3 -4 -3)
                     (v/vec3 3 0 -5)
                     (v/vec3 3 1 -5)
                     (v/vec3 3 3 -3)])

(defn create-world [threadpool]
  [(bezier-spline/->Surface (-> (bezier-spline/->BezierSpatialTree control-points 8)
                                (bezier-spline/build))
                            threadpool
                            control-points
                            #_(->Metal (v/vec3 0.8 0.6 0.2) 0.0) (->Dielectric 10.5)
                            0.00001 100 5)
   (->Sphere (v/vec3 -0.5 2.0 -3.0) 0.5 (->Lambertian (v/vec3 0.1 0.2 0.5)))
   (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 (->Lambertian (v/vec3 0.8 0.8 0.0)))
   (->Sphere (v/vec3 -2.0 1.0 -1.0) 0.5 (->Metal (v/vec3 0.8 0.6 0.2) 0.0))
   (->Sphere (v/vec3 -2.0 2.0 -1.0) 0.5 (->Dielectric 1.5))
   (->Sphere (v/vec3 -1.0 1.0 -1.0) -0.45 (->Dielectric 1.5))])

(defn color
  ([ray world] (color ray world 50))
  ([^Ray ray world ^long depth]
   (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
     (let [[attenuation scattered] (scatter (.material world-hit) ray world-hit)]
       (if (and attenuation (pos? depth))
         (v/emult attenuation (color scattered world (dec depth)))
         zero))
     (let [^Vec3 unit (v/normalize (.direction ray))
           t (* 0.5 (inc (.y unit)))]
       (v/interpolate v1 v2 t)))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 200)

(def img (p/pixels nx ny))

(def camera (positionable-camera (v/vec3 -2 2 0.5) (v/vec3 -1 1.5 -1) (v/vec3 0 1 0) 80 (/ (double nx) ny)))
#_(def camera (positionable-camera (v/vec3 -2 2 1) (v/vec3 0 0 -1) (v/vec3 0 1 0) 20 (/ (double nx) ny)))

(defn compute []
  (cp/with-shutdown! [bezier-threadpool (-> (Runtime/getRuntime)
                                            (.availableProcessors)
                                            (cp/threadpool))
                      horizontal-pixels-threadpool (-> (Runtime/getRuntime)
                                                       (.availableProcessors)
                                                       (cp/threadpool))]
    (let [world (create-world bezier-threadpool)]
      (time (dotimes [j ny]
              (println (str "Line: " j))
              (cp/pdoseq horizontal-pixels-threadpool [i (range nx)]
                (let [col (reduce v/add zero
                                  (repeatedly samples #(let [u (/ (+ (r/drand) i) nx)
                                                             v (/ (+ (r/drand) j) ny)
                                                             r (get-ray camera u v)]
                                                         (color r world))))]
                  (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                         (v/sqrt)
                                                         (v/mult 255.0))))))))))

(compute)

(u/show-image img)

;; (save img "results/rt-in-weekend/camera2-fov20.jpg")

