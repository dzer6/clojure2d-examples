(ns rt-in-weekend.ch12-random-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.camera :refer :all]
            [rt-in-weekend.material :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [rt-in-weekend.bezier.normalized-trees-forest :as normalized-trees-forest]
            [rt-in-weekend.bezier.complex-object :as bezier-complex-object]
            [com.climate.claypoole :as cp])
  (:import [fastmath.vector Vec3 Vec2]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(def ^:const zero (v/vec3 0.0 0.0 0.0))

(defn random-scene [threadpool]
  (let [utah-teapot-forest (->> "http://www.holmes3d.net/graphics/teapot/teapotrim.bpt"
                                (normalized-trees-forest/create threadpool 7))
        teacup-forest (->> "http://www.holmes3d.net/graphics/teapot/teacup.bpt"
                           (normalized-trees-forest/create threadpool 7))
        world [(->Sphere (v/vec3 0 -1000 0) 1000 (->Lambertian (v/vec3 0.5 0.5 0.5)))

               (bezier-complex-object/create threadpool utah-teapot-forest
                                             0.001 1000 5
                                             (v/vec3 0 1 0) 1.7 (->Dielectric 1.5))

               (bezier-complex-object/create threadpool utah-teapot-forest
                                             0.001 1000 5
                                             (v/vec3 -4 1 0.5) 1.7 (->Lambertian (v/vec3 0.4 0.2 0.1)))

               (bezier-complex-object/create threadpool utah-teapot-forest
                                             0.001 1000 5
                                             (v/vec3 4 1 -0.5) 1.7 (->Metal (v/vec3 0.7 0.6 0.5) 0.0))]]
    (vec (concat world
                 (for [^int a (range -11 11 5)
                       ^int b (range -11 11 5)
                       :let [center (v/vec3 (+ a (r/drand 0.9)) 0.4 (+ b (r/drand 0.9)))
                             choose-mat (r/drand)]
                       :when (> (v/mag (v/sub center (v/vec3 4 0.4 0))) 0.9)]
                   (let [material (cond
                                    (< choose-mat 0.6) (->Lambertian (v/vec3 (m/sq (r/drand))
                                                                             (m/sq (r/drand))
                                                                             (m/sq (r/drand))))
                                    (< choose-mat 0.9) (->Metal (v/vec3 (r/drand 0.5 1.0)
                                                                        (r/drand 0.5 1.0)
                                                                        (r/drand 0.5 1.0)) (r/drand))
                                    :else (->Dielectric (r/drand 1 2)))]
                     (bezier-complex-object/create threadpool teacup-forest
                                                   0.001 1000 5
                                                   center 0.4 material)))))))

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

(def ^:const ^int nx 400)
(def ^:const ^int ny 200)
(def ^:const ^int samples 2)

(def img (p/pixels nx ny))

(def camera
  (let [lookfrom (v/vec3 13 2 4)
        lookat (v/vec3 0 0 0)]
    (positionable-camera lookfrom lookat (v/vec3 0 1 0) 20 (/ (double nx) ny) 0.1 20.0)))

(def r2-seq (vec (take samples (r/jittered-sequence-generator :r2 2 0.5))))

(def window (show-window {:canvas  (canvas nx ny)
                          :draw-fn (fn [c _ _ _] (p/set-canvas-pixels! c img))
                          :fps     1}))

(defn compute []
  (cp/with-shutdown! [threadpool (-> (Runtime/getRuntime)
                                     (.availableProcessors)
                                     (cp/threadpool))]
    (let [world (random-scene threadpool)]
      (time (dotimes [j ny]
              (println (str "Line: " j))
              (cp/pdoseq threadpool [i (range nx)]
                (let [p (v/vec2 i j)
                      col (reduce v/add zero
                                  (map #(let [^Vec2 pp (v/add % p)
                                              u (/ (.x pp) nx)
                                              v (/ (.y pp) ny)
                                              r (get-ray camera u v)]
                                          (color r world)) r2-seq))]
                  (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                         (v/sqrt)
                                                         (v/mult 255.0))))))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(->> (System/currentTimeMillis)
     (format "./target/ch12-random-scene_%s.jpg")
     (save img))
