(ns aoc2023.aoc18
  (:require [aoc.colls :as c]
            [aoc.math :as math]
            [clojure.string :as str]))

(def test-input "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc18.txt"))

(def dirs {"0" [0 1] "R" [0 1] "2" [0 -1] "L" [0 -1] "3" [-1 0] "U" [-1 0] "1" [1 0] "D" [1 0]})

(defn parse-move [s]
  (let [[_ dir cnt color] (re-find #"(\w+) (\d+) \(#(\w+)\)" s)]
    (c/map-of (dirs dir) (parse-long cnt) color)))

(defn parse-input [in] (->> in str/split-lines (mapv parse-move)))

(defn p2-input-adjust [{:keys [color]}]
  {:cnt (Long/parseLong (subs color 0 5) 16)
   :dir (dirs (subs color 5))})

(defn p [in] (reduce + (vals (math/rectilinear-area in))))

(defn p1 [in] (p (parse-input in)))
(defn p2 [in] (p (map p2-input-adjust (parse-input in))))
