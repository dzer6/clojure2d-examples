(ns rt-in-weekend.bezier.patch
  (:require [rt-in-weekend.ray :refer :all]
            [rt-in-weekend.hitable :refer :all]
            [rt-in-weekend.protocols :refer :all]
            [rt-in-weekend.sphere :refer :all]
            [fastmath.vector :as vec]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure.core.memoize :as memo]
            [com.rpl.specter :as specter]
            [rt-in-weekend.util :as ut])
  (:import [rt_in_weekend.ray Ray]
           (fastmath.vector Vec3)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

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
  (assoc! node k v))

(defn get-field [node k]
  (get node k))

(defn get-field-fn [k]
  (fn [node]
    (get-field node k)))

(def MAP-VECTOR-NODES
  (specter/recursive-path [] p
    (specter/cond-path (fn [item]
                         (and (vector? item)
                              (not (instance? Vec3 item)))) [specter/ALL p]
                       map? (specter/continue-then-stay specter/MAP-VALS p))))

(defn transform-node [field func node]
  (specter/transform [MAP-VECTOR-NODES (specter/must field)]
                     func
                     node))

(defn add-spheres [node]
  (specter/transform [MAP-VECTOR-NODES]
                     (fn [{:keys [center radius] :as value}]
                       (->> (->Sphere center radius nil)
                            (assoc value :sphere)))
                     node))

(defn as-map [n]
  (let [node (persistent! n)]
    {:center     (:center node)
     :radius     (:radius node)
     :u          (:u node)
     :v          (:v node)
     :last-level (:last-level node)
     :children   (some->> (:children node)
                          (mapv as-map))}))

(defn new-node [& _]
  (transient {}))

;;;

(def Tu [0.0 0.5 0.0 0.5])
(def Tv [0.0 0.0 0.5 0.5])

(defn reverse-div ^double [^double a ^double b]
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
  (let [[^double u ^double v _] (reduce (partial eval-uv-by-path path)
                                        (v/vec3 0.0 0.0 1.0)
                                        (-> ^long levels-number dec range))
        four-points [(bezier-s control-points u v)
                     (bezier-s control-points (+ u ^double part-size) v)
                     (bezier-s control-points u (+ v ^double part-size))
                     (bezier-s control-points (+ u ^double part-size) (+ v ^double part-size))]
        Center (->> four-points
                    (reduce v/add)
                    (ut/flip v/div 4))
        R (->> four-points
               (map (partial v/dist Center))
               (reduce m/fast-max))]
    (set-field node :u u)
    (set-field node :v v)
    (set-field node :last-level true)
    (set-field node :center Center)
    (set-field node :radius R)))

(defn four-spheres-into-one-sphere [node]
  (let [children (:children node)
        Center (->> children
                    (map (get-field-fn :center))
                    (reduce v/add)
                    (ut/flip v/div 4))
        R (->> children
               (sort-by (get-field-fn :radius))
               (last)
               (ut/flip get-field :radius))
        r (->> children
               (map (comp (partial v/dist Center)
                          (get-field-fn :center)))
               (reduce m/fast-max))]
    (set-field node :last-level false)
    (set-field node :center Center)
    (set-field node :radius (+ ^double R ^double r))))

;;;

(defn transient-children [node]
  (->> (transient [])
       (set-field node :children)))

(defn persistent-children [{:keys [children] :as node}]
  (->> (persistent! children)
       (set-field node :children)))

(defn add-child [{:keys [children]} child]
  (conj! children child))

(defn build-tree [control-points levels-number part-size level path node]
  (if (= level (dec ^long levels-number))
    (four-points-into-one-sphere control-points levels-number part-size path node)
    (do
      (transient-children node)
      (doseq [i (range 4)]
        (let [child (new-node)]
          (add-child node child)
          (build-tree control-points levels-number part-size
                      (inc ^long level) (conj path i) child)))
      (persistent-children node)
      (four-spheres-into-one-sphere node))))

(defrecord BezierSpatialTree [^long levels-number control-points]
  BuildableProto
  (build [_]
    (let [part-size (/ 1.0 (* (dec levels-number) (dec levels-number)))
          root (new-node)]
      (build-tree control-points levels-number part-size 0 [] root)
      (as-map root))))

;;;

(defn determ ^double [^Vec3 a ^Vec3 b ^Vec3 c]
  (+ (* (.x a) (- (* (.y b) (.z c)) (* (.z b) (.y c))))
     (* (.x b) (- (* (.z a) (.y c)) (* (.y a) (.z c))))
     (* (.x c) (- (* (.y a) (.z b)) (* (.z a) (.y b))))))

(defn within-the-margin-of-error [^Vec3 vect ^double epsil]
  (let [x (.x vect)
        y (.y vect)
        z (.z vect)]
    (and (< (m/abs x) epsil)
         (< (m/abs y) epsil)
         (< (m/abs z) epsil))))

(defn boundary-conditions-are-met [u v t t-min t-max]
  (and (<= 0.0 ^double u 1.0)
       (<= 0.0 ^double v 1.0)
       (<= ^double t-min ^double t ^double t-max)))

(defn newton–raphson-iteration [control-points
                                epsil
                                ^Vec3 ray
                                t-min
                                t-max
                                iteration-limit
                                material
                                ^Vec3 uvt
                                i]
  (let [u (.x uvt)
        v (.y uvt)
        t (.z uvt)
        ^Vec3 patp (point-at-parameter ^Ray ray t)
        ^Vec3 f (-> (bezier-s control-points u v)
                    (vec/sub patp))
        ^Vec3 f-u (bezier-su control-points u v)
        ^Vec3 f-v (bezier-sv control-points u v)
        ^Vec3 ray-sub-dir (vec/sub (.direction ^Ray ray))
        dn (- (determ f-u f-v ray-sub-dir))]
    (if (or (= i (dec ^long iteration-limit))
            (within-the-margin-of-error f epsil))
      (-> (when (boundary-conditions-are-met u v t t-min t-max)
            (->HitData t patp (-> (vec/cross f-u f-v) (vec/normalize)) material))
          (reduced))
      (v/vec3 (+ u (/ (determ f f-v ray-sub-dir) dn))
              (+ v (/ (determ f-u f ray-sub-dir) dn))
              (+ t (/ (determ f-u f-v f) dn))))))

;;;

(defrecord Surface [spatial-tree-root-node control-points epsil iteration-limit sample-size material]
  HitableProto
  (hit [_ ray t-min t-max]
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
                    (reduce (partial newton–raphson-iteration control-points epsil ray t-min t-max iteration-limit material)
                            (v/vec3 u v t-min)
                            (range iteration-limit))))
             (filter some?)
             (seq)
             (min-hit))))

;;;