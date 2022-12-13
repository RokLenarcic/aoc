(ns aoc.arrays
  (:require
    [aoc.chars :refer [char-square]]
    [clojure.walk :as walk]
    [medley.core :as m]
    [ubergraph.core :as u]))

(defn size* [rect-array]
  [(count rect-array) (count (rect-array 0))])

(defn rot
  "Rotates coordinates 90 deg, p is [row col]"
  [p dir]
  (case dir
    (:R \R) [(p 1) (- (p 0))]
    (:L \L) [(- (p 1)) (p 0)]))

(defn rot3
  "Clockwise"
  [[x y z] axis ^long deg]
  (if (zero? deg)
    [x y z]
    (let [cos (case deg 45 1/2 90 0 135 -1/2 180 -1 225 -1/2 270 0 315 1/2 360 1)
          sin (case deg 45 1/2 90 1 135 1/2 180 0 225 -1/2 270 -1 315 -1/2 360 0)]
      (case axis
        :x [x (- (* cos y) (* sin z)) (+ (* sin y) (* cos z))]
        :y [(+ (* cos x) (* sin z)) y (- (* cos z) (* sin x))]
        :z [(- (* cos x) (* sin y)) (+ (* sin x) (* cos y)) z]))))

(def dirs (vec (take 4 (iterate #(rot % :R) [0 1]))))
(def diagonal-dirs (vec (take 4 (iterate #(rot % :R) [1 1]))))

(defn dirs-dim [dimensions]
  (letfn [(inner [d]
            (for [x [-1 0 1]
                  v (if (<= d 1) [[]] (inner (dec d)))]
              (conj v x)))]
    (filterv #(not (every? zero? %)) (inner dimensions))))

(defn p-sum
  "Sum two points"
  [p1 p2]
  (mapv + (or p1 [0 0]) (or p2 [0 0])))

(defn adjacent* [dirs p]
  (map (partial p-sum p) dirs))

(defn- rasterize-low
  "Uses Bresenham's alg"
  [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        yi (Long/signum dy)
        dy (* yi dy)]
    (loop [x x1 y y1 d (- (* 2 dy) dx) acc []]
      (let [acc' (conj acc [x y])
            d' (+ d (* 2 (if (pos-int? d) (- dy dx) dy)))
            y' (cond-> y (pos-int? d) (+ yi))]
        (if (= x x2)
          acc'
          (recur (inc x) y' d' acc'))))))

(defn rasterize
  [[x1 y1] [x2 y2]]
  (if (< (abs (- y2 y1)) (abs (- x2 x1)))
    (if (> x1 x2)
      (vec (reverse (rasterize-low x2 y2 x1 y1)))
      (rasterize-low x1 y1 x2 y2))
    (if (> y1 y2)
      (vec (reverse (map (comp vec reverse) (rasterize-low y2 x2 y1 x1))))
      (map (comp vec reverse) (rasterize-low y1 x1 y2 x2)))))

(defn in-array? [arr coord]
  (some? (get-in arr coord))
  #_(if-some [row (get-in arr (butlast coord))]
    (contains? row (last coord))
    false))

(defn ray
  "Returns coords in a direction from a spot in kd array, includes origin"
  [rect-arr dir coord]
  (take-while (partial in-array? rect-arr) (iterate #(p-sum % dir) coord)))

(defn vray
  "Returns values in a direction from a spot in kd array, includes origin"
  [rect-arr dir coord]
  (map #(get-in rect-arr %) (ray rect-arr dir coord)))

(defn fan-out
  "Returns coords fanning out of a spot in kd array, returns seq of seq of coords, one subseq for one ray,
  does not include origin."
  [rect-arr dirs coord]
  (for [dir dirs] (ray rect-arr dir (p-sum dir coord))))

(defn rectangle-coordinates
  ([[min-row min-col] [max-row max-col]]
   (concat (mapcat #(list [min-row %] [max-row %]) (range min-col (inc max-col)))
           (mapcat #(list [% min-col] [% max-col]) (range 1 max-row)))))

(defn border-coordinates [[rows cols]] (rectangle-coordinates [0 0] [(dec rows) (dec cols)]))

(defn margin-coordinates
  "Coordinates just outside the rectangle"
  ([p-min p-max] (rectangle-coordinates (mapv dec p-min) (mapv inc p-max)))
  ([[rows cols]] (rectangle-coordinates [-1 -1] [rows cols])))

(defn rows-to-cols [arr]
  (vec (apply map vector arr)))

(defn find-coords [arr pred]
  (if (vector? arr)
    (->> (range (count arr))
         (mapcat (fn [idx]
                   (when-let [res (find-coords (arr idx) pred)]
                     (map (partial cons idx) res))))
         (remove nil?))
    (when (pred arr) '[()])))

(defn update-all* [prefix arr f]
  (if (vector? (arr 0))
    (into [] (map-indexed #(update-all* (conj prefix %1) %2 f)) arr)
    (into [] (map-indexed #(f (conj prefix %1) %2)) arr)))

(defprotocol IArrayKd
  (dim [this] "Array dimensions")
  (val-at [this p])
  (as-nested-vec [this])
  (find-val [this v] "Returns a seq of points")
  (filter-arr [this pred] "Returns map of p->val that match (pred val)")
  (adjacent [this p] "Returns map of p->val of adjacent squares")
  (update-val [this p f])
  (update-all [this f] "Function is (point, value) -> new-value")
  (set-val [this p v]))

(defrecord ArrayKd [arr dirs]
  IArrayKd
  (dim [this] (reduce #(conj %1 (count %2)) [] (take-while some? (iterate #(let [f (% 0)] (when (vector? f) f)) arr))))
  (val-at [this p] (get-in arr p))
  (as-nested-vec [this] arr)
  (find-val [this v] (map vec (find-coords arr #(= % v))))
  (filter-arr [this pred] (let [coords (find-coords arr pred)]
                            (zipmap (map vec coords)
                                    (map #(val-at this %) coords))))
  (adjacent [this p] (let [coords (filter (partial in-array? arr) (map p-sum (repeat p) dirs))]
                       (zipmap coords (map #(val-at this %) coords))))
  (update-val [this p f] (->ArrayKd (update-in arr p f) dirs))
  (update-all [this f] (->ArrayKd (update-all* [] arr f) dirs))
  (set-val [this p v] (->ArrayKd (assoc-in arr p v) dirs)))

(defn bounds
  "Returns [[minx,miny], [maxx, maxy]]"
  [points]
  (reduce (fn [[mins maxes :as ret] p]
            (if ret
              [(mapv min p mins) (mapv max p maxes)]
              [p p]))
          nil
          points))

(defrecord MapKd [m dirs]
  IArrayKd
  (dim [this] (let [[mins maxes] (bounds m)] (mapv - maxes mins)))
  (val-at [this p] (get m p))
  (as-nested-vec [this]
    (letfn [(vec-of [mins maxes el]
              (vec (repeat (- (first maxes) (first mins)) el)))
            (construct-empty [mins maxes]
              (when (seq mins)
                (vec-of mins maxes (construct-empty (next mins) (next maxes)))))]
      (let [[mins maxes] (bounds (keys m))]
        (reduce-kv
          (fn [acc k v]
            (assoc-in acc (map - k mins) v))
          (construct-empty mins (map inc maxes))
          m))))
  (find-val [this v] (reduce-kv (fn [ps p v*] (if (= v* v) (conj ps p) ps)) [] m))
  (filter-arr [this pred] (m/filter-vals pred m))
  (adjacent [this p] (let [coords (map p-sum (repeat p) dirs)] (select-keys m coords)))
  (update-val [this p f] (->MapKd (update m p f) dirs))
  (update-all [this f] (->MapKd (m/map-kv-vals f m) dirs))
  (set-val [this p v] (->MapKd (assoc m p v) dirs)))

(defn print-tf
  "Print green or black square if true/false"
  [x] (if x (:green char-square) (:black char-square)))

(defn print-2d
  ([arr] (print-2d arr print-tf))
  ([arr ch-fn]
   (doseq [row arr :let [_ (println)]
           col row]
     (print (ch-fn col)))
   (println)))

(defn print-2d-map
  ([m] (print-2d-map m print-tf))
  ([m ch-fn]
   (let [[[minx miny] [maxx maxy]] (bounds (keys m))]
     (print (format "[%s, %s]" minx miny))
     (doseq [row (range minx (inc maxx))
             :let [_ (println)]
             col (range miny (inc maxy))]
       (print (ch-fn (m [row col]))))
     (println (format "[%s, %s]" maxx maxy)))))

(defn re-center
  "Move map coordinates so they begin at 0,0"
  [m]
  (let [[mins] (bounds (keys m))]
    (update-keys m #(p-sum % (map - mins)))))

(defn rotated-and-mirrored
  "Returns all rotations and mirrored rotations of m."
  [m]
  (let [mirrored (update-keys m #(update % 0 -))]
    (concat (take 4 (iterate (fn [m] (re-center (update-keys m #(rot % :R)))) m))
            (take 4 (iterate (fn [m] (re-center (update-keys m #(rot % :R)))) (re-center mirrored))))))

(defn array->graph
  [arr]
  (let [nodes (filter-arr arr any?)
        ug-nodes (map (fn [[p v]] [p {:v v}]) nodes)
        ug-edges (for [n (keys nodes)
                       adj (adjacent arr n)]
                   [n (key adj)])]
    (apply u/digraph (concat ug-nodes ug-edges))))

(defn node-val [g node] (u/attr g node :v))


(defn resolve-paths
  "Some Loom algs return nested maps that represent a path {a {b {c 3},
  this returns map {[a c] 3}"
  [paths]
  (reduce-kv
    (fn [acc k v]
      (let [m (walk/postwalk
                #(if (map-entry? %)
                   (if (number? (val %)) % (val %))
                   %) v)]
        (merge acc (update-keys m #(vector k %)))))
    {}
    paths))

(defn simulate-investment [per-month apr]
  (let [monthly-apr (Math/pow apr (double (/ 1 12)))]
    (iterate #(* monthly-apr (+ per-month %)) 0)))

(defn invest-years [per-month apr years]
  (long (nth (simulate-investment per-month apr) (* years 12))))
