(ns aoc2023.aoc24
  (:require [aoc.colls :as c]
            [aoc.inputs :as in]
            [clojure.string :as str]
            [medley.core :as m])
  (:import (com.microsoft.z3 Context)))

(def test-input "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc24.txt"))

(defn parse-input [in] (->> in str/split-lines (mapv #(vec (map vec (split-at 3 (mapv bigint (in/parse-numbers true %))))))))

(defn linear-equation [[x y] [dx dy]]
   (let [k (/ dy dx)]
     [k (- y (* k x))]))

(defn intersect-vertical [x [x2 y2 :as p2] v2]
  (if (zero? (first v2))
    (if (= x x2) p2)
    (let [[a b] (linear-equation p2 v2)
          y (+ b (* a x))]
      (when (= y y2) [x y]))))

(defn intersects2 [[[x1 y1 :as p1] v1] [[x2 y2 :as p2] v2]]
  (cond
    (zero? (first v1)) (intersect-vertical x1 p2 v2)
    (zero? (first v2)) (intersect-vertical x2 p1 v1)
    :else
    (let [[a c] (linear-equation p1 v1)
          [b d] (linear-equation p2 v2)]
      (if (= a b)
        (when (= c d) p1)
        (let [x (/ (- d c) (- a b))]
          [x (+ (* a x) c)])))))

(defn within-bounds? [p min-coord max-coord]
  (every? #(<= min-coord % max-coord) p))

(defn in-future?
  "Tells if intersection point is in the future of hailstone path"
  [[p v] p']
  (let [idx (m/find-first #(not (zero? (v %))) (range (count v)))]
    (if (pos? (v idx))
      (<= (p idx) (p' idx))
      (<= (p' idx) (p idx)))))

(defn p1 [in]
  (let [hailstones (parse-input in)
        pairs (for [i1 (range (count hailstones))
                    i2 (range (inc i1) (count hailstones))
                    :let [h1 (hailstones i1) h2 (hailstones i2)
                          inter (intersects2 h1 h2)]
                    :when (and inter (in-future? h1 inter) (in-future? h2 inter))
                    ]
                 inter)]
    (c/count-matching #(and % (within-bounds? % 200000000000000 400000000000000)) pairs)))

(defn p2 [in]
  (with-open [context (Context.)]
    (let [hailstones (parse-input in)]
      (.parseSMTLIB2String
        context))))

(comment
  ;; used online Z3 solver https://jfmc.github.io/z3-play
  (declare-const rx Int)
         (declare-const ry Int)
         (declare-const rz Int)
         (declare-const rvx Int)
         (declare-const rvy Int)
         (declare-const rvz Int)
         (declare-const t0 Int)
         (declare-const t1 Int)
         (declare-const t2 Int)
         (declare-const gold Int)
         (assert (= (+ rx (* t0  rvx)) (+ 280749343264517 (* t0 46))))
         (assert (= (+ ry (* t0  rvy)) (+ 310056912736219 (* t0 7))))
         (assert (= (+ rz (* t0  rvz)) (+ 232684093145441 (* t0 69))))

         (assert (= (+ rx (* t1  rvx)) (+ 305207571533357 (* t1 -33))))
         (assert (= (+ ry (* t1  rvy)) (+ 298697252599459 (* t1 -13))))
         (assert (= (+ rz (* t1  rvz)) (+ 311819844132065 (* t1 -13))))

         (assert (= (+ rx (* t2  rvx)) (+ 278604047935731 (* t2 41))))
         (assert (= (+ ry (* t2  rvy)) (+ 298887079420155 (* t2 21))))
         (assert (= (+ rz (* t2  rvz)) (+ 444483112092287 (* t2 -392))))
         (assert (= (+ rx ry rz) gold))
         (check-sat)
         (get-model))
