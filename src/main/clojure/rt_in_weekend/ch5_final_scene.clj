(ns rt-in-weekend.ch5-final-scene
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [rt-in-weekend.material :refer :all]
            [fastmath.core :as m]
            [com.climate.claypoole :as cp]
            [rt-in-weekend.util :as ut]
            [rt-in-weekend.bezier.normalized-trees-forest :as normalized-trees-forest]
            [rt-in-weekend.bezier.complex-object :as bezier-complex-object]
            [clj-tuple :as ct])
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

(defn color [^Ray ray world]
  (if-let [^HitData world-hit (hit-list world ray 0.0 Double/MAX_VALUE)]
    (v/mult (v/add (.normal world-hit) one) 127.5)
    (let [^Vec3 unit (v/normalize (.direction ray))
          t (* 0.5 (inc (.y unit)))]
      (v/interpolate v1 v2 t))))

(def ^:const ^int nx 800)
(def ^:const ^int ny 400)

(def img (p/pixels nx ny))

(def window (show-window {:canvas  (canvas nx ny)
                          :draw-fn (fn [c _ _ _] (p/set-canvas-pixels! c img))
                          :fps     1}))

(defn compute []
  (cp/with-shutdown! [threadpool (-> (Runtime/getRuntime)
                                     (.availableProcessors)
                                     (cp/threadpool))]
    (let [world (create-world threadpool)]
      (time (try
              (dotimes [j ny]
                (when (zero? (mod j 50)) (println (str "Line: " j)))
                (cp/pdoseq threadpool [i (range nx)]
                  (let [u (/ (double i) nx)
                        v (/ (double j) ny)
                        r (->Ray orig (v/add lower-left-corner
                                             (v/add (v/mult horizontal u)
                                                    (v/mult vertical v))))]
                    (p/set-color! img i
                                  (- (dec ny) j)
                                  (color r world)))))
              (catch Exception e
                (.printStackTrace e)))))))

(try
  (compute)
  (catch Exception e
    (.printStackTrace e)))

(->> (System/currentTimeMillis)
     (format "./target/ch5-final-scene_%s.jpg")
     (save img))
