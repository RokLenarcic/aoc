(ns aoc2015.aoc17
  (:require
    [aoc.colls :as c]
    [clojure.math.combinatorics :as comb]
    [clojure.string :as str]))

(def input (->> (slurp "/Users/roklenarcic/aoc/aoc15/aoc17.txt")
                str/trim str/split-lines
                (map parse-long)))

(defn p1 [in]
  (->> (comb/subsets in)
       (filter #(= 150 (c/sum-of %)))
       count))

(defn p2 [in]
  (let [valid-arragements (->> (comb/subsets in) (filter #(= 150 (c/sum-of %))))
        min-elements (reduce min (map count valid-arragements))]
    (count (filter #(= min-elements (count %)) valid-arragements))))
