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
            [rt-in-weekend.bezier.complex-object :as bezier-complex-object]
            [rt-in-weekend.bezier.normalized-trees-forest :as normalized-trees-forest]
            [com.climate.claypoole :as cp]
            [clj-tuple :as ct]
            [rt-in-weekend.util :as ut])
  (:import [fastmath.vector Vec3]
           [rt_in_weekend.ray Ray]
           [rt_in_weekend.hitable HitData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const v1 (v/vec3 1.0 1.0 1.0))
(def ^:const v2 (v/vec3 0.5 0.7 1.0))

(defn create-world [threadpool]
  (let [epsil 0.001
        iteration-limits-sample-sizes (ct/hash-map (ct/vector 1 4) (ct/vector 100 1)
                                                   (ct/vector 5 30) (ct/vector 1000 3)
                                                   (ct/vector 31 90) (ct/vector 5000 10)
                                                   (ct/vector 91 150) (ct/vector 10000 15)
                                                   (ct/vector 151 300) (ct/vector 30000 20)
                                                   (ct/vector 301 1000000) (ct/vector 50000 25))
        tree-levels-number 8
        rotate-teapot (comp (partial ut/flip v/axis-rotate (v/vec3 0 1 0) m/QUARTER_PI)
                            (partial ut/flip v/axis-rotate (v/vec3 -1 0 0) m/HALF_PI))
        utah-teapot-forest (->> "http://www.holmes3d.net/graphics/teapot/teapotrim.bpt"
                                (normalized-trees-forest/create threadpool tree-levels-number rotate-teapot))
        the-teapot (partial bezier-complex-object/create threadpool utah-teapot-forest
                            epsil iteration-limits-sample-sizes)]
    [(->Sphere (v/vec3 0.0 -100.5 -1.0) 100.0 nil)
     #_(->Sphere (v/vec3 0.0 0.0 -1.0) 0.5 nil)
     (the-teapot (v/vec3 0.0 0.0 -1.0) 0.7 nil)]))

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

(def window (show-window {:canvas  (canvas nx ny)
                          :draw-fn (fn [c _ _ _] (p/set-canvas-pixels! c img))
                          :fps     1}))

(defn compute []
  (cp/with-shutdown! [threadpool (-> (Runtime/getRuntime)
                                     (.availableProcessors)
                                     (cp/threadpool))]
    (let [world (create-world threadpool)]
      (time (dotimes [j ny]
              #_(when (zero? (mod j 50)) (println (str "Line: " j)))
              (println (str "Line: " j))
              (cp/pdoseq threadpool [i (range nx)]
                (let [col (reduce v/add (v/vec3 0.0 0.0 0.0)
                                  (repeatedly samples #(let [u (/ (+ (r/drand) i) nx)
                                                             v (/ (+ (r/drand) j) ny)
                                                             r (get-ray default-camera u v)]
                                                         (color r world))))]
                  (p/set-color! img i (- (dec ny) j) (-> (v/div col samples)
                                                         (v/sqrt)
                                                         (v/mult 255.0))))))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(->> (System/currentTimeMillis)
     (format "./target/ch7-diffuse_%s.jpg")
     (save img))
