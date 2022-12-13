(ns aoc2020.aoc13
  (:require [clojure.string :as str]
            [aoc.math :as math]
            [medley.core :as m]))

(defn parse-input [in]
  (let [[t bus-lines] (-> in str/trim str/split-lines)]
    {:t (parse-long t)
     :bus-lines (map parse-long (str/split bus-lines #","))}))

(def test-input (parse-input "939\n7,13,x,x,59,x,31,19"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc13.txt")))

(defn p1 [{:keys [t bus-lines]}]
  (let [waiting (for [bus bus-lines :when bus]
                  [(- (- (mod t bus) bus)) bus])]
    (->> waiting
         (apply min-key first)
         (reduce *))))

(defn combine [[t step] [idx bus]]
  [(m/find-first #(zero? (mod (+ % idx) bus))
                 (iterate #(+ % step) t))
   (math/lcm bus step)])

(defn p2 [{:keys [bus-lines]}]
  (first (reduce combine [0 1] (filter second (m/indexed bus-lines)))))
