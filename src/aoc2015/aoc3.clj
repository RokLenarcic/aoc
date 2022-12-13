(ns aoc2015.aoc3
  (:require [aoc.arrays :as a]))

(def dirs (zipmap [\> \v \< \^] a/dirs))
(def input (slurp "/Users/roklenarcic/aoc/aoc15/aoc3.txt"))
(defn visited [in] (reductions (fn [p c] (a/p-sum p (dirs c))) [0 0] in))

(defn p1 [in] (count (distinct (visited in))))

(defn p2 [in] (count (distinct (mapcat visited (a/rows-to-cols (partition 2 in))))))
