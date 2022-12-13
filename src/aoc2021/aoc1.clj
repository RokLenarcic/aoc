(ns aoc2021.aoc1
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]))

(def test-input (inputs/parse-numbers "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"))
(def input (inputs/parse-numbers (slurp "/Users/roklenarcic/aoc/aoc21/aoc1.txt")))

(defn p1 [in] (c/count-matching pos? (map - (rest in) in)))
(defn p2 [in] (p1 (map + in (rest in) (rest (rest in)))))
