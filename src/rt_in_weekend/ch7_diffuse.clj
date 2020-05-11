(ns rt-in-weekend.ch7-diffuse
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.camera :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [rt-in-weekend.bezier-spline :as bezier-spline])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(def control-points [(v/vec3 -1 -1 -3)
                     (v/vec3 -1 0 -5)
                     (v/vec3 -1 1 -5)
                     (v/vec3 -1 2 -3)

                     (v/vec3 0 -1 -3)
                     (v/vec3 0 0 -4)
                     (v/vec3 0 1 -9)
                     (v/vec3 0 2 -3)

                     (v/vec3 1 -1 -3)
                     (v/vec3 1 0 -9)
                     (v/vec3 1 1 -4)
                     (v/vec3 1 2 -3)

                     (v/vec3 2 -1 -3)
                     (v/vec3 2 0 -5)
                     (v/vec3 2 1 -5)
                     (v/vec3 2 2 -3)])

(defn create-world []
  [#_(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5 nil)
   (bezier-spline/->Surface (-> (bezier-spline/->BezierSpatialTree control-points 8)
                                (bezier-spline/build))
                            control-points
                            nil 0.00001 100 5)
   (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 nil)])

(defn random-in-unit-sphere []
  (let [v (v/vec3 (r/drand -1.0 1.0) (r/drand -1.0 1.0) (r/drand -1.0 1.0))]
    (if (< ^double (v/magsq v) 1.0) v (recur))))

(defn color [^Ray ray world]
  (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
    (let [target (v/add (v/add (random-in-unit-sphere) (.normal world-hit)) (.p world-hit))]
      (v/mult (color (->Ray (.p world-hit) (v/sub target (.p world-hit))) world) 0.5))
    (let [^Vec3 unit (v/normalize (.direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 200)

(def img (p/pixels nx ny))

(defn compute []
  (let [world (create-world)]
    (time (dotimes [j ny]
            #_(when (zero? (mod j 50)) (println (str "Line: " j)))
            (println (str "Line: " j))
            (dotimes [i nx]
              (let [col (reduce v/add (v/vec3 0.0 0.0 0.0)
                                (repeatedly samples #(let [u (/ (+ (r/drand) i) nx)
                                                           v (/ (+ (r/drand) j) ny)
                                                           r (get-ray default-camera u v)]
                                                       (color r world))))]
                (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                       (v/sqrt)
                                                       (v/mult 255.0)))))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(u/show-image img)

(->> (System/currentTimeMillis)
     (format "./target/ch7-diffuse_%s.jpg")
     (save img))
