(ns aoc2016.aoc23
  (:require [aoc2016.bunnycode :as bc]))

(def test-input "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a")

(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc23.txt"))

(defn p1 [in] (:a (bc/run-bunnycode in {:a 7})))
(defn p2 [in] (:a (bc/run-bunnycode in {:a 12})))
