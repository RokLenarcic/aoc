(ns aoc2023.aoc13
  (:require [aoc.inputs :as in]
            [aoc.colls :as c]
            [aoc.arrays :as a]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc13.txt"))

(defn diff-num [board row]
  (let [diffs (map not= (mapcat board (range row -1 -1))
                        (mapcat board (range (inc row) (count board))))]
    (c/count-matching true? diffs)))

(defn mirror-row [board diffs-required] (->> (range (dec (count board)))
                                             (m/find-first #(= diffs-required (diff-num board %)))
                                             ;; i++ but 0 if no mirror row
                                             ((fnil inc -1))))

(defn mirror-score [board diffs-required]
  (let [row-score (mirror-row board diffs-required)
        col-score (mirror-row (a/rows-to-cols board) diffs-required)]
    (+ (* 100 row-score) col-score)))

(defn parse-input [in] (map #(in/char-array2d % (fn [x] (= x \#))) (str/split in #"\n\n")))
(defn p [in diffs-required] (c/sum-of #(mirror-score % diffs-required) (parse-input in)))

(defn p1 [in] (p in 0))
(defn p2 [in] (p in 1))
