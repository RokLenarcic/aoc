(ns aoc.math
  (:require [aoc.arrays :as a])
  (:import (java.util TreeMap)))

(defn gcd "(gcd a b) returns the greatest common divisor of a and b" [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))
    (loop [a (abs a) b (abs b)]
      (if (zero? b) a (recur b (mod a b))))))

(defn lcm
  "(lcm a b) returns the least common multiple of a and b"
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "lcm requires two integers")))
  (cond (zero? a) 0
        (zero? b) 0
        :else (abs (* b (quot a (gcd a b))))))

(defn combinations-repeat
  "Returns all possible combinations with repeating items. For instance all possible
  splits of picking one of 4 items 100 times."
  [[item & more] total-items]
  (let [downstream (if more (combinations-repeat more total-items) [{}])]
    (for [d downstream
          i (range (- (inc total-items) (reduce + (vals d))))]
      (assoc d item i))))

(defn factors [n]
  (loop [i 2 n* n ret '()]
    (if (> i (bit-shift-right n* 1))
      (cons n* ret)
      (if (zero? (unchecked-remainder-int n* i))
        (recur i (unchecked-divide-int n* i) (cons i ret))
        (recur (unchecked-inc-int i) n* ret)))))

(defn sum-of-factors [n]
  (loop [i 2 n* n acc 2 upper 1 lower 1]
    (if (= n* 1)
      (quot (unchecked-multiply upper (unchecked-dec acc))
            (unchecked-multiply lower (unchecked-dec i)))
      (if (zero? (unchecked-remainder-int n* i))
        (recur i (unchecked-divide-int n* i) (unchecked-multiply acc i) upper lower)
        (let [i* (unchecked-inc i)
              i* (int (if (> i* (bit-shift-right n* 1)) n* i*))]
          (if (= acc i)
            (recur i* n* i* upper lower)
            (recur i* n* i*
                   (unchecked-multiply upper (unchecked-dec acc))
                   (unchecked-multiply lower (unchecked-dec i)))))))))

(defn interval-seq-map
  "Create a map of intervals from [start end) -> val, returning TreeMap"
  [interval-maps]
  (reduce-kv (fn [^TreeMap m [start end] v]
               (.put m start [true v])
               (.put m end [false v])
               m)
             (TreeMap.)
             interval-maps))

#_(defn merge-interval-maps [f m1 m2]
  (let [ret (TreeMap.)
        items (loop [[e1 :as s1] m1 [e2 :as s2] m2 acc []]
                (if (neg-int? (compare e1 e2))
                  (recur (next s1) s2 (conj acc [0 e1]))
                  (recur s1 (next s2) (conj acc [1 e2]))))]
    (loop [[[stream [k [start v]] :as it] & more] (rest items)
           [open-stream [open-start? open-v?]] (first items)]
      (when it
        (if (= open-stream stream')

          (do (.put ret k [start? v])
              (recur it (next more)))
          )))
    ret))

(defn intersection
  "Intersect 2 intervals, returns nil or a vec of:
  - intersecting interval
  - 0-2 remaining intervals from first arg
  - 0-2 remaining intervals from second arg
  intervals are closed"
  [interval other-interval]
  (let [[i-min i-max] interval
        [o-min o-max] other-interval
        inter [(max o-min i-min) (min o-max i-max)]
        valid? (fn [x] (<= (x 0) (x 1)))]
    (when (valid? inter)
      [inter
       (filterv valid? [[i-min (dec o-min)] [(inc o-max) i-max]])
       (filterv valid? [[o-min (dec i-min)] [(inc i-max) o-max]])])))

(defn intersect-shape
  "Intersect two orthogonal shapes, described by two points , returns nil or a vec of
  - intersecting shape
  - shapes sliced from first shape
  - shapes sliced from second shape
  closed intervals"
  [shape other-shape]
  (when-let [[common only1 only2] (intersection (mapv first shape) (mapv first other-shape))]
    ;; final step
    (if (= 1 (count (first shape)))
      [(mapv vector common) (mapv #(mapv vector %) only1) (mapv #(mapv vector %) only2)]
      (let [add-intersection-to-subdims (fn [subdims] (mapv #(vec (cons %1 %2)) common subdims))
            limit-shape-to-interval (fn [shape interval] (mapv #(assoc %1 0 %2) shape interval))]
        (when-let [[common-core common1 common2] (intersect-shape (mapv #(subvec % 1) shape)
                                                                  (mapv #(subvec % 1) other-shape))]
          [(add-intersection-to-subdims common-core)
           (into (mapv (partial limit-shape-to-interval shape) only1)
                 (mapv add-intersection-to-subdims common1))
           (into (mapv (partial limit-shape-to-interval other-shape) only2)
                 (mapv add-intersection-to-subdims common2))])))))

(defn closest-point
  "Closest point in a AABB to P"
  [[min-p max-p] p]
  (letfn [(f [mins maxs p]
              (if p
                (cons (min (max (first p) (first mins)) (first maxs))
                      (f (next mins) (next maxs) (next p)))
                '()))]
      (vec (f min-p max-p p))))

(defn closest-point-3d
  "Closest point in a box to p, 3d, about 5 times faster than the general variant"
  [[[minx miny minz] [maxx maxy maxz]] [x y z]]
  [(min (max x minx) maxx)
   (min (max y miny) maxy)
   (min (max z minz) maxz)])

(defn greater-int
  "For a double find the next integer larger than the x, returning double type. 2.0 -> 3.0"
  [x] (let [x' (Math/ceil x)] (if (= x' x) (inc x) x')))

(defn lesser-int
  "For a double find the next integer smaller than the x, returning double type."
  [x] (let [x' (Math/floor x)] (if (= x' x) (dec x) x')))

(defn polygon-expand
  "Given a closed form with 90 deg corners, going clock-wise, change coordinates to give
  a polygon that encompasses also the edge. Required when using algos that
  work with polygons with no border width.

  Last corner shouldn't be first corner repeated."
  [corners]
  (mapv (fn [[prev-row prev-col] [row col] [next-row next-col]]
          (if (< prev-row next-row)
            (if (< prev-col next-col)
              [row (inc col)]
              [(inc row) (inc col)])
            (if (< prev-col next-col)
              [row col]
              [(inc row) col])))
        (cons (last corners) corners)
        corners
        (rest (cycle corners))))

(defn shoelace-formula
  "Shoelace formula, returns area and perimeter, use pick's theorem for integer coordinates.

  Watch out for clockwise direction. Works with points or with maps of {:dir :cnt}"
  [coll]
  (if (map? (first coll))
    (shoelace-formula (a/spans->corners coll [0 0]))
    (let [l (last coll)
          coll (if (= (first coll) l) (next coll) coll)
          dist (fn [p1 p2] (let [diffs (mapv - p2 p1)]
                             (Math/sqrt (reduce + (mapv * diffs diffs)))))]
      (-> (zipmap [:area :perimeter]
                  (reduce (fn [[area perimeter [row1 col1 :as p1]] [row2 col2 :as p2]]
                            [(+ area (- (* col1 row2) (* col2 row1)))
                             (+ perimeter (dist p1 p2))
                             p2])
                          [0 0 l] coll))
          (update :area #(abs (/ % 2)))))))

(defn rectilinear-area
  "Calculates inner area and inner point in a rectilinear polygon, using Pick's Theorem

  Input is either a vector of corner points or a coll of maps with keys :dir and :cnt"
  [coll]
  (let [{:keys [area perimeter]} (shoelace-formula coll)]
    {:perimeter (long perimeter)
     :inside (- (inc area) (/ (long perimeter) 2))}))

(defn linear-equations
  "Solve a system of linear equations:
  a11*x1 + a12*x2 + a13*x3 = y1
  a21*x1 + a22*x2 + a23*x3 = y2

  Returns vector [x1 x2 x3 ...]"
  [])
