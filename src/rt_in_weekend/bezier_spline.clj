(ns rt-in-weekend.bezier-spline
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.sphere :as sp]
            [fastmath.vector :as vec]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure.core.memoize :as memo])
  (:import [rt_in_weekend.ray Ray]
           (fastmath.vector Vec3)
           (rt_in_weekend.hitable HitData)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;;;

(defn flip [f & xs]
  (apply f (reverse xs)))

;;;

(defn B [^long i ^double t]
  (let [s (- 1.0 t)]
    (condp = i
      0 (* s s s)
      1 (* 3 t s s)
      2 (* 3 t t s)
      3 (* t t t))))

(defn Bp [^long i ^double t]
  (condp = i
    0 (* -3 (- 1.0 t) (- 1.0 t))
    1 (* 3 (- 1.0 (* t (- 4.0 (* 3 t)))))
    2 (* t (- 6.0 (* 9.0 t)))
    3 (* 3 t t)))

(def memo-control-point (memoize (fn [control-points ^long i ^long j]
                                   (nth control-points (+ (* i 4) j)))))

(defn bezier-s [control-points u v]
  (reduce (fn [^Vec3 t ^long i]
            (-> (reduce (fn [^Vec3 s ^long j]
                          (->> (B j v)
                               (vec/mult (memo-control-point control-points i j))
                               (vec/add s)))
                        (vec/vec3)
                        (range 4))
                (vec/mult (B i u))
                (vec/add t)))
          (vec/vec3)
          (range 4)))

(defn bezier-su [control-points u v]
  (reduce (fn [^Vec3 t ^long i]
            (-> (reduce (fn [^Vec3 s ^long j]
                          (->> (B j v)
                               (vec/mult (memo-control-point control-points i j))
                               (vec/add s)))
                        (vec/vec3)
                        (range 4))
                (vec/mult (Bp i u))
                (vec/add t)))
          (vec/vec3)
          (range 4)))

(defn bezier-sv [control-points u v]
  (reduce (fn [^Vec3 t ^long i]
            (-> (reduce (fn [^Vec3 s ^long j]
                          (->> (Bp j v)
                               (vec/mult (memo-control-point control-points i j))
                               (vec/add s)))
                        (vec/vec3)
                        (range 4))
                (vec/mult (B i u))
                (vec/add t)))
          (vec/vec3)
          (range 4)))

;;;

(defn set-field [node k v]
  (-> (get node k)
      (reset! v)))

(defn get-field [node k]
  (-> (get node k)
      (deref)))

(defn get-field-fn [k]
  (fn [node]
    (get-field node k)))

(defprotocol ConvertibleProto
  (as-map [object]))

(defrecord SphericalTreeNode [center
                              radius
                              u
                              v
                              last-level
                              children]
  ConvertibleProto
  (as-map [_]
    {:sphere     (sp/->Sphere @center @radius nil)
     :center     @center
     :radius     @radius
     :u          @u
     :v          @v
     :last-level @last-level
     :children   (map as-map @children)})
  Object
  (toString [self]
    (with-out-str
      (clojure.pprint/pprint (as-map self)))))

(defn new-node [& _]
  (->SphericalTreeNode (atom (v/vec3))
                       (atom 0.0)
                       (atom 0.0)
                       (atom 0.0)
                       (atom false)
                       (atom [])))

;;;

(defprotocol SpatialTreeProto
  (build [object]))

;;;

(def Tu [0.0 0.5 0.0 0.5])
(def Tv [0.0 0.0 0.5 0.5])

(defn reverse-div [^double a ^double b]
  (/ b a))

(defn eval-param-by-path-flag [^long flag coefficient-vec ^double h ^double param]
  (->> flag
       (nth coefficient-vec)
       (reverse-div h)
       (+ param)))

(def memo-eval-param-by-path-flag (memo/ttl eval-param-by-path-flag :ttl/threshold 5000))

(defn eval-uv-by-path [quadruple-tree-path ^Vec3 uvh ^long i]
  (let [flag (get quadruple-tree-path i)
        u (.x uvh)
        v (.y uvh)
        h (.z uvh)]
    (v/vec3 (memo-eval-param-by-path-flag flag Tu h u)
            (memo-eval-param-by-path-flag flag Tv h v)
            (* 2.0 h))))

(defn four-points-into-one-sphere [control-points levels-number part-size path node]
  (let [[u v _] (reduce (partial eval-uv-by-path path)
                        (v/vec3 0.0 0.0 1.0)
                        (-> levels-number dec range))
        four-points (->> [[u v]
                          [(+ u part-size) v]
                          [u (+ v part-size)]
                          [(+ u part-size) (+ v part-size)]]
                         (map (partial apply (partial bezier-s control-points))))
        Center (->> four-points
                    (reduce v/add)
                    (flip v/div 4))
        R (->> four-points
               (map (partial v/dist Center))
               (reduce m/fast-max))]
    (set-field node :u u)
    (set-field node :v v)
    (set-field node :last-level true)
    (set-field node :center Center)
    (set-field node :radius R)))

(defn four-spheres-into-one-sphere [node]
  (let [children (get-field node :children)
        Center (->> children
                    (map (get-field-fn :center))
                    (reduce v/add)
                    (flip v/div 4))
        R (->> children
               (sort-by (get-field-fn :radius))
               (last)
               (flip get-field :radius))
        r (->> children
               (map (comp (partial v/dist Center)
                          (get-field-fn :center)))
               (reduce m/fast-max))]
    (set-field node :last-level false)
    (set-field node :center Center)
    (set-field node :radius (+ R r))))

;;;

(defn add-child [node child]
  (swap! (:children node) conj child))

(defn build-tree [control-points levels-number part-size level path node]
  (if (= level (dec levels-number))
    (four-points-into-one-sphere control-points levels-number part-size path node)
    (do
      (doseq [i (range 4)]
        (let [child (new-node)]
          (add-child node child)
          (build-tree control-points levels-number part-size
                      (inc level) (conj path i) child)))
      (four-spheres-into-one-sphere node))))

(defrecord BezierSpatialTree [control-points levels-number]
  SpatialTreeProto
  (build [_]
    (let [part-size (/ 1.0 (* (dec levels-number) (dec levels-number)))
          root (new-node)]
      (build-tree control-points levels-number part-size 0 [] root)
      (as-map root))))

;;;

(defprotocol BezierProto
  (intersect-with-ray [object ray initial-u initial-v t-min t-max]))

(defn determ [^Vec3 a ^Vec3 b ^Vec3 c]
  (+ (* (.x a) (- (* (.y b) (.z c)) (* (.z b) (.y c))))
     (* (.x b) (- (* (.z a) (.y c)) (* (.y a) (.z c))))
     (* (.x c) (- (* (.y a) (.z b)) (* (.z a) (.y b))))))

(defn newton–raphson-iteration [control-points epsil ray t-min t-max material iteration-limit ^Vec3 uvt i]
  (let [u (.x uvt)
        v (.y uvt)
        t (.z uvt)
        patp (point-at-parameter ^Ray ray t)
        f (-> (bezier-s control-points u v)
              (vec/sub patp))
        f-u (bezier-su control-points u v)
        f-v (bezier-sv control-points u v)
        ray-sub-dir (vec/sub (.direction ^Ray ray))
        dn (- (determ f-u f-v ray-sub-dir))]
    (if (or (= i (dec iteration-limit))
            (and (-> (.x f) (m/abs) (< epsil))
                 (-> (.y f) (m/abs) (< epsil))
                 (-> (.z f) (m/abs) (< epsil))))
      (-> (when (and (<= 0 u 1) (<= 0 v 1) (<= t-min t t-max))
            (->HitData t patp (-> (vec/cross f-u f-v) (vec/normalize)) material))
          (reduced))
      (v/vec3 (+ u (/ (determ f f-v ray-sub-dir) dn))
              (+ v (/ (determ f-u f ray-sub-dir) dn))
              (+ t (/ (determ f-u f-v f) dn))))))

;;;

(defn min-hit [hits]
  (reduce (fn [^HitData a ^HitData b]
            (if (< (.t a) (.t a))
              a
              b))
          hits))

(defrecord Surface [spatial-tree-root-node control-points material epsil iteration-limit sample-size]
  HitableProto
  (hit [object ray t-min t-max]
    (some->> spatial-tree-root-node
             (tree-seq (fn [{:keys [last-level sphere]}]
                         (and (not last-level)
                              (some?
                                (hit sphere ray t-min t-max))))
                       :children)
             (seq)                                          ; short circuit that works with some->>
             (filter :last-level)
             (filter (fn [{:keys [sphere]}]
                       (some?
                         (hit sphere ray t-min t-max))))
             (shuffle)
             (take sample-size)
             (map (fn [{:keys [u v]}]
                    (intersect-with-ray object ray u v t-min t-max)))
             (filter some?)
             (seq)
             (min-hit)))
  BezierProto
  (intersect-with-ray [_ ray initial-u initial-v t-min t-max]
    (reduce (partial newton–raphson-iteration control-points epsil ray t-min t-max material iteration-limit)
            (v/vec3 initial-u initial-v t-min)
            (range iteration-limit))))

;;;