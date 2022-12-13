(ns aoc2015.aoc2
  (:require [clojure.string :as str]))

(defn wrapping-size [sizes]
  (let [[x y z] (sort sizes)]
    (+ (* 3 x y) (* 2 y z) (* 2 x z))))

(defn ribbon-size [sizes]
  (let [[x y z] (sort sizes)]
    (+ (* 2 x) (* 2 y) (* x y z))))

(def input (->> (slurp "/Users/roklenarcic/aoc/aoc15/aoc2.txt")
                str/split-lines
                (map #(map parse-long (str/split % #"x")))))

(defn p1 [in] (transduce (map wrapping-size) + in))
(defn p2 [in] (transduce (map ribbon-size) + in))
