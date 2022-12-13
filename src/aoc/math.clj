(ns aoc.math
  (:require [aoc.colls :as c]))

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
