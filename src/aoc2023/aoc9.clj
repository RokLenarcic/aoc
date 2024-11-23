(ns aoc2023.aoc9
  (:require [aoc.inputs :as in]
            [clojure.string :as str]))

(def test-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc9.txt"))

(defn last-diff [s]
  (->> (iterate #(mapv - (next %) %) s)
       (take-while #(not (every? zero? %)))
       (transduce (map last) +)))

(defn p [in pre-proc]
  (->> in str/split-lines
       (mapv #(pre-proc (in/parse-numbers true %)))
       (transduce (map last-diff) +)))

(defn p1 [in] (p in identity))
(defn p2 [in] (p in reverse))
