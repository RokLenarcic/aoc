(ns aoc2018.aoc6
  (:require [aoc.inputs :as inputs]
            [aoc.arrays :as a]
            [clojure.string :as str]))

(def test-input "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc6.txt")))

(defn parse-input [in]
  (mapv (comp vec inputs/parse-numbers) (str/split-lines in)))

(defn distances [points p]
  (->> points
       (map-indexed #(vector %1 (reduce + (mapv (comp abs -) p %2))))
       (sort-by second)))

(defn point-areas [points]
  (into {}
        (let [[[xmin ymin] [xmax ymax]] (a/bounds points)]
          (for [x (range (dec xmin) (+ 2 xmax))
                y (range (dec ymin) (+ 2 ymax))
                :let [[shortest other] (distances points [x y])]
                ;; shortest must be unique
                :when (not= (second shortest) (second other))]
            [[x y] (first shortest)]))))

(defn inner-area [points]
  (let [[[xmin ymin] [xmax ymax]] (a/bounds points)]
    (for [x (range (dec xmin) (+ 2 xmax))
          y (range (dec ymin) (+ 2 ymax))
          :when (< (transduce (map second) + (distances points [x y])) 10000)]
      [x y])))

(defn area-bounds [points] (apply a/margin-coordinates (a/bounds points)))

(defn p1 [in]
  (let [points (parse-input in)
        areas (point-areas points)
        infinite (into #{} (map areas) (area-bounds points))]
    (reduce max (vals (apply dissoc (frequencies (vals areas)) infinite)))))

(defn p2 [in] (count (inner-area (parse-input in))))
