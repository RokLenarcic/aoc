(ns aoc2016.aoc12
  (:require [aoc2016.bunnycode :as bc]))

(def test-input "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a")

(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc12.txt"))

(defn p1 [in] (:a (bc/run-bunnycode in {})))
(defn p2 [in] (:a (bc/run-bunnycode in {:c 1})))
