(ns aoc2018.aoc1
  (:require [clojure.string :as str]))

(def test-input "+1, -2, +3, +1")
(def input (map read-string (str/split-lines (slurp "/Users/roklenarcic/aoc/aoc18/aoc1.txt"))))

(defn p1 [in] (reduce + in))

(defn p2 [in]
  (loop [f 0
         seen #{}
         items (cycle in)]
    (if (seen f)
      f
      (recur (int (+ f (first items))) (conj seen f) (rest items)))))
