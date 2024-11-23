(ns aoc2023.aoc4
  (:require [aoc.colls :as c]
            [aoc.inputs :as in]
            [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc4.txt"))

(defn parse-card [c]
  (let [[_ win have] (str/split c #":|\|")]
    (count (set/intersection (into #{} (in/parse-numbers win)) (into #{} (in/parse-numbers have))))))

(defn parse-input [in] (mapv parse-card (str/split-lines in)))

(defn p1 [in]
  (->> (parse-input in)
       (filter pos-int?)
       (c/sum-of #(Math/pow 2.0 (double (dec %))))))

(defn p2 [in]
  (let [counts (parse-input in)]
    (->> (for [[i cnt] (m/indexed (parse-input in))
               j (range (inc i) (+ (inc i) cnt))]
           [i j])
         (reduce (fn [acc [from to]] (update acc to + (acc from)))
                 (mapv (constantly 1) counts))
         (reduce +))))
