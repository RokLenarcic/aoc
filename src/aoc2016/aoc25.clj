(ns aoc2016.aoc25
  (:require [medley.core :as m]))

(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc25.txt"))

(def input2 2555)
(defn matching? [n] (re-matches #"(10)+" (.toString (BigInteger/valueOf n) 2)))

(defn p1 [in]
  (m/find-first #(matching? (+ in %)) (range)))
