(ns aoc2021.aoc9
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [aoc.colls :as c]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(defn parse-input [in]
  (a/->ArrayKd (inputs/char-array2d in (comp parse-long str)) a/dirs))

(defn minimums [in]
  (let [arr (parse-input in)]
    (-> arr
        (a/update-all (fn [p v] (when (every? #(< v %) (vals (a/adjacent arr p))) v)))
        (a/filter-arr identity))))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")
(def input (slurp "/Users/roklenarcic/aoc/aoc21/aoc9.txt"))

(defn p1 [in]
  (let [mins (minimums in)]
    (transduce (map inc) + (vals mins))))

(defn p2 [in]
  (let [arr (parse-input in)
        blocked (a/filter-arr arr #(= 9 %))
        components (-> (a/array->graph arr)
                       (u/remove-nodes* (keys blocked))
                       (alg/connected-components))]
    (->> components
         (map set)
         (sort-by (comp - count))
         (map count)
         (take 3)
         (reduce *))))
