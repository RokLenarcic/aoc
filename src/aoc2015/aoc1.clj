(ns aoc2015.aoc1
  (:require [medley.core :as m]))

(def input (slurp "/Users/roklenarcic/aoc/aoc15/aoc1.txt"))
(defn floor [s]
  (let [f (frequencies s)]
    (- (f \() (f \)))))

(defn p1 [in] (floor in))
(defn p2 [in]
  (m/find-first #(neg-int? (floor (subs in 0 %))) (range 2 (count in))))
