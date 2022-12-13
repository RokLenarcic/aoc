(ns aoc.aoc1
  (:require [aoc.inputs :as in]
            [aoc.colls :as c]))

(defn elves-calories []
  (map c/sum-of (in/blocks (slurp "/Users/roklenarcic/aoc/aoc1_1.txt") :item-fn parse-long)))

(defn a1 []
  (reduce max (elves-calories)))

(defn a2 []
  (c/sum-of (take 3 (reverse (sort (elves-calories))))))
