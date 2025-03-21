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
            [com.climate.claypoole :as cp]
            [rt-in-weekend.util :as ut]
            [clj-tuple :as ct])
  (:import [fastmath.vector Vec3 Vec2]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def v1 (v/vec3 1.0 1.0 1.0))
(def v2 (v/vec3 0.5 0.7 1.0))

(def zero (v/vec3 0.0 0.0 0.0))

(defn rand-material [^double choose-mat]
  (cond
    (< choose-mat 0.6) (->Lambertian (v/vec3 (m/sq (r/drand))
                                             (m/sq (r/drand))
                                             (m/sq (r/drand))))
    (< choose-mat 0.9) (->Metal (v/vec3 (r/drand 0.5 1.0)
                                        (r/drand 0.5 1.0)
                                        (r/drand 0.5 1.0)) (r/drand))
    :else (->Dielectric (r/drand 1 2))))

(defn random-scene [threadpool]
  (let [epsil                         0.001
        iteration-limits-sample-sizes (ct/hash-map (ct/vector 1 4) (ct/vector 100 1)
                                                   (ct/vector 5 30) (ct/vector 1000 3)
                                                   (ct/vector 31 90) (ct/vector 5000 10)
                                                   (ct/vector 91 150) (ct/vector 10000 15)
                                                   (ct/vector 151 300) (ct/vector 30000 20)
                                                   (ct/vector 301 1000000) (ct/vector 50000 25))
        tree-levels-number            7
        rotate-teapot                 (comp (partial ut/flip v/axis-rotate (v/vec3 0 1 0) m/QUARTER_PI)
                                            (partial ut/flip v/axis-rotate (v/vec3 -1 0 0) m/HALF_PI))
        utah-teapot-forest            (->> "http://www.holmes3d.net/graphics/teapot/teapotrim.bpt"
                                           (normalized-trees-forest/create threadpool tree-levels-number rotate-teapot))
        rotate-teacup                 (partial ut/flip v/axis-rotate (v/vec3 0 1 0) m/HALF_PI)
        teacup-forest                 (->> "http://www.holmes3d.net/graphics/teapot/teacup.bpt"
                                           (normalized-trees-forest/create threadpool tree-levels-number rotate-teacup))
        the-teapot                    (partial bezier-complex-object/create threadpool utah-teapot-forest
                                               epsil iteration-limits-sample-sizes)
        teacup                        (partial bezier-complex-object/create threadpool teacup-forest
                                               epsil iteration-limits-sample-sizes)
        world                         [(->Sphere (v/vec3 0 -1000 0) 1000 (->Lambertian (v/vec3 0.5 0.5 0.5)))
                                       (the-teapot (v/vec3 -4 1 0.5) 1.9 (->Lambertian (v/vec3 0.4 0.2 0.1)))
                                       (the-teapot (v/vec3 0 1 0) 1.9 (->Dielectric 1.5))
                                       (the-teapot (v/vec3 4 1 -0.5) 1.9 (->Metal (v/vec3 0.7 0.6 0.5) 0.0))]
        teacups                       (cp/pfor threadpool [^int a (range -11 11 2)
                                                           ^int b (range -11 11 2)
                                                           :let [center (v/vec3 (+ a (r/drand 0.9)) 0.2 (+ b (r/drand 0.9)))
                                                                 choose-mat (r/drand)]
                                                           :when (> (v/mag (v/sub center (v/vec3 4 0.2 0))) 0.9)]
                                               (->> (rand-material choose-mat)
                                                    (teacup center 0.4)))
        small-spheres                 (cp/pfor threadpool [^int a (range -11 11 3)
                                                           ^int b (range -11 11 3)
                                                           :let [center (v/vec3 (+ a (r/drand 0.9)) 0.2 (+ b (r/drand 0.9)))
                                                                 choose-mat (r/drand)]
                                                           :when (> (v/mag (v/sub center (v/vec3 4 0.2 0))) 0.9)]
                                               (->> (rand-material choose-mat)
                                                    (->Sphere center 0.2)))]
    (->> (concat world
                 teacups
                 small-spheres)
         (vec))))

(defn color
  ([ray world] (color ray world 50))
  ([^Ray ray world ^long depth]
   (if-let [^HitData world-hit (hit-list world ray 0.001 Double/MAX_VALUE)]
     (let [[attenuation scattered] (scatter (.material world-hit) ray world-hit)]
       (if (and attenuation (pos? depth))
         (v/emult attenuation (color scattered world (dec depth)))
         zero))
     (let [^Vec3 unit (v/normalize (.direction ray))
           t          (* 0.5 (inc (.y unit)))]
       (v/interpolate v1 v2 t)))))

(def nx 1000)
(def ny 500)
(def samples 200)

(def img (p/pixels nx ny))

(def camera
  (let [lookfrom (v/vec3 13 2 4)
        lookat   (v/vec3 0 0 0)]
    (positionable-camera lookfrom lookat (v/vec3 0 1 0) 20 (/ (double nx) ny) 0.1 20.0)))

(def r2-seq (vec (take samples (r/jittered-sequence-generator :r2 2 0.5))))

(def window (show-window {:canvas  (canvas nx ny)
                          :draw-fn (fn [c _ _ _] (p/set-canvas-pixels! c img))
                          :fps     1}))

(defn compute []
  (cp/with-shutdown! [threadpool (cp/threadpool 200
                                                :thread-priority Thread/MIN_PRIORITY)]
    (let [world (time (random-scene threadpool))]
      (time
        (->> (range ny)
             (map (fn [j]
                    (map (fn [i]
                           (ct/vector i j))
                         (range nx))))
             (flatten)
             (partition 2)
             (shuffle)
             (cp/pmap threadpool
                      (fn [[i j]]
                        (let [p   (v/vec2 i j)
                              col (reduce v/add zero
                                          (map #(let [^Vec2 pp (v/add % p)
                                                      u        (/ (.x pp) nx)
                                                      v        (/ (.y pp) ny)
                                                      r        (get-ray camera u v)]
                                                  (color r world)) r2-seq))]
                          (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                                 (v/sqrt)
                                                                 (v/mult 255.0))))))
             (doall))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(->> (System/currentTimeMillis)
     (format "./target/ch12-random-scene_%s.jpg")
     (save img))
