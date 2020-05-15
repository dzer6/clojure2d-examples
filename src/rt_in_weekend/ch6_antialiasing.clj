(ns rt-in-weekend.ch6-antialiasing
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
            [fastmath.protocols :as pr]
            [rt-in-weekend.bezier.complex-object :as bezier-complex-object]
            [rt-in-weekend.bezier.normalized-trees-forest :as normalized-trees-forest]
            [com.climate.claypoole :as cp])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def v1 (v/mult (v/vec3 1.0 1.0 1.0) 255.0))
(def v2 (v/mult (v/vec3 0.5 0.7 1.0) 255.0))
(def one (v/vec3 1.0 1.0 1.0))

(defn create-world [threadpool]
  (let [utah-teapot-forest (->> "http://www.holmes3d.net/graphics/teapot/teapotrim.bpt"
                                (normalized-trees-forest/create threadpool 8))]
    [(->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 nil)
     #_(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5 nil)
     (bezier-complex-object/create threadpool utah-teapot-forest
                                   0.000001 10000 5
                                   (v/vec3 0.0 0.0 -1.0) 0.5 nil)]))

(defn color [^Ray ray world]
  (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
    (pr/mult (pr/add (.normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (.direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)
(def ^:const ^int samples 150)

(def img (p/pixels nx ny))

(def window (show-window {:canvas  (canvas nx ny)
                          :draw-fn (fn [c _ _ _] (p/set-canvas-pixels! c img))
                          :fps     1}))
(defn compute []
  (cp/with-shutdown! [threadpool (-> (Runtime/getRuntime)
                                     (.availableProcessors)
                                     (cp/threadpool))]
    (let [world (create-world threadpool)]
      (time (dotimes [j ny]
              (when (zero? (mod j 50)) (println (str "Line: " j)))
              (cp/pdoseq threadpool [i (range nx)]
                (let [col (reduce v/add (v/vec3 0.0 0.0 0.0)
                                  (repeatedly samples #(let [u (/ (+ (r/drand) i) nx)
                                                             v (/ (+ (r/drand) j) ny)
                                                             r (get-ray default-camera u v)]
                                                         (color r world))))]
                  (p/set-color! img i (- (dec ny) j) (v/div col samples)))))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(->> (System/currentTimeMillis)
     (format "./target/ch6-antialiasing_%s.jpg")
     (save img))
