(ns rt-in-weekend.ch5-final-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.bezier-spline :as bezier-spline]
            [fastmath.core :as m]
            [com.climate.claypoole :as cp])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/mult (v/vec3 1.0 1.0 1.0) 255.0))
(def ^:const v2 (v/mult (v/vec3 0.5 0.7 1.0) 255.0))
(def ^:const lower-left-corner (v/vec3 -2.0 -1.0 -1.0))
(def ^:const horizontal (v/vec3 4.0 0.0 0.0))
(def ^:const vertical (v/vec3 0.0 2.0 0.0))
(def ^:const orig (v/vec3 0.0 0.0 0.0))
(def ^:const one (v/vec3 1.0 1.0 1.0))

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
  [(->Sphere (v/vec3 0.0 0.0 -1.0) 0.25 nil)
   (bezier-spline/->Surface (-> (bezier-spline/->BezierSpatialTree control-points 8)
                                (bezier-spline/build))
                            control-points
                            nil 0.00001 100 8)
   (->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 nil)])

(defn color [^Ray ray world]
  (if-let [^HitData world-hit (hit-list world ray 0.0 Double/MAX_VALUE)]
    (v/mult (v/add (.normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (.direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)

(def img (p/pixels nx ny))

(defn compute []
  (cp/with-shutdown! [horizontal-pixels-threadpool (-> (Runtime/getRuntime)
                                                       (.availableProcessors)
                                                       (cp/threadpool))]
    (let [world (create-world)]
      (time (try
              (dotimes [j ny]
                (when (zero? (mod j 50)) (println (str "Line: " j)))
                (cp/pdoseq horizontal-pixels-threadpool [i (range nx)]
                  (let [u (/ (double i) nx)
                        v (/ (double j) ny)
                        r (->Ray orig (v/add lower-left-corner (v/add (v/mult horizontal u) (v/mult vertical v))))]
                    (p/set-color! img i (- (dec ny) j) (color r world)))))
              (catch Exception e
                (.printStackTrace e)))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(u/show-image img)

(->> (System/currentTimeMillis)
     (format "./target/ch5-final-scene_%s.jpg")
     (save img))
