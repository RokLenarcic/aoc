(ns aoc2021.aoc7
  (:require [aoc.colls :as c]
            [aoc.inputs :as inputs]))

(def test-input (inputs/parse-numbers true "16,1,2,0,4,2,7,1,2,14"))
(def input (inputs/parse-numbers true (slurp "/Users/roklenarcic/aoc/aoc21/aoc7.txt")))

(defn fuel-for-pos [x pos] (c/sum-of #(abs (- pos %)) x))
(defn fuel-for-pos' [x pos] (c/sum-of #(let [diff (abs (- pos %))]
                                         (quot (* diff (inc diff)) 2)) x))

(defn least-fuel-consumed [in cost-fn]
  (reduce min (map #(cost-fn in %) (range (reduce min in) (apply max in)))))

(defn p1 [in] (least-fuel-consumed in fuel-for-pos))
(defn p2 [in] (least-fuel-consumed in fuel-for-pos'))
