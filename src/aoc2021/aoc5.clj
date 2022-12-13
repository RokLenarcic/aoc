(ns aoc2021.aoc5
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as inputs]
    [clojure.string :as str]
    [medley.core :as m]))

(defn parse-line [l]
  (let [[x1 y1 x2 y2] (inputs/parse-numbers l)]
    {:straight? (or (= x1 x2) (= y1 y2))
     :ps (a/rasterize [x1 y1] [x2 y2])}))

(defn parse-input [in]
  (->> in str/trim str/split-lines (map parse-line)))

(def test-input (parse-input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21/aoc5.txt")))

(defn count-intersections [in filter-pred]
  (->> in
       (filter filter-pred)
       (mapcat :ps)
       (reduce #(update %1 %2 (fnil inc 0)) {})
       (m/filter-vals #(< 1 %))
       count))

(defn p1 [in] (count-intersections in :straight?))
(defn p2 [in] (count-intersections in any?))
