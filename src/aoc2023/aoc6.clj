(ns aoc2023.aoc6
  (:require [aoc.inputs :as in]
            [aoc.math :as math]
            [clojure.string :as str]))

(def test-input "Time:      7  15   30\nDistance:  9  40  200")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc6.txt"))

(defn solve [t win]
  (let [inner (Math/sqrt (double (- (* t t) (* 4 (double win)))))]
    [(/ (- inner t) -2) (/ (+ inner t) 2)]))

(defn solutions [t win]
  (let [[lower upper] (solve t win)]
    (inc (- (math/lesser-int upper) (math/greater-int lower)))))

(defn parse-input [in] (map in/parse-numbers (str/split-lines in)))
(defn p [[times winning]] (int (apply * (mapv solutions times winning))))

(defn p1 [in] (p (parse-input in)))
(defn p2 [in] (->> (parse-input in)
                   (mapv #(vector (parse-long (apply str %))))
                   p))
