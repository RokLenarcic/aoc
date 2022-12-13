(ns aoc2020.aoc25)

(def input [19241437 17346587])
(def test-input [5764801 17807724])
(defn xfs [subject] (iterate #(mod (unchecked-multiply subject %) 20201227) 1))

(defn crack-loop-size [public-key]
  (loop [i 0 [x & more] (xfs 7)]
    (when x (if (= public-key x) i (recur (unchecked-inc i) more)))))

(defn p1 [in]
  (let [[loop-size1 loop-size2] (pmap crack-loop-size in)]
    (nth (xfs (first in)) loop-size2)))
