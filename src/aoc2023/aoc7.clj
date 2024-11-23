(ns aoc2023.aoc7
  (:require [clojure.string :as str]))

(def test-input "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc7.txt"))

(defn hand-replace [in j]
  (reduce #(apply str/replace %1 %2) in [[\T \u] [\J j] [\Q \w] [\K \x] [\A \y]]))

(defn hand-rank
  "0 high card, 1 pair, 2 two pair, 3 tris, 4 full house, 5 poker, 6 six of a kind"
  [h]
  (let [f (frequencies h)]
    (case (count f)
      5 0
      4 1
      3 (if (some #(= 3 %) (vals f)) 3 2)
      2 (case (val (first f))
          (4 1) 5 (2 3) 4)
      1 6)))

(defn hand-rank2 [h]
  (let [f (frequencies h)]
    (if (contains? f \*)
      (case (count f)
        5 1
        4 3
        3 (if (= [2 2] (vals (dissoc f \*))) 4 5)
        2 6
        1 6)
      (hand-rank h))))

(defn parse-input [in] (mapv #(update (str/split % #"\s+") 1 parse-long) (str/split-lines in)))

(defn p [in hand-rank-fn]
  (->> (parse-input in)
       (mapv #(update % 0 (fn [h] [(hand-rank-fn h) h])))
       (sort)
       (transduce (map-indexed (fn [i [_ bid]] (* bid (inc i)))) +)))

(defn p1 [in] (p (hand-replace in \v) hand-rank))
(defn p2 [in] (p (hand-replace in \*) hand-rank2))
