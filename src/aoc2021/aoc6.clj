(ns aoc2021.aoc6
  (:require [aoc.inputs :as inputs]))

(defn round [m]
  (reduce-kv
    (fn [acc k v]
      (case k
        0 (-> acc (assoc 8 v) (update 6 (fnil + 0) v))
        7 (update acc 6 (fnil + 0) v)
        (assoc acc (dec k) v)))
    {}
    m))

(def test-input (frequencies (inputs/parse-numbers "3,4,3,1,2")))
(def input (frequencies (inputs/parse-numbers (slurp "/Users/roklenarcic/aoc/aoc21/aoc6.txt"))))

(defn fishes-after-rounds [in rounds] (reduce + (vals (nth (iterate round in) rounds))))

(defn p1 [in] (fishes-after-rounds in 80))
(defn p2 [in] (fishes-after-rounds in 256))
