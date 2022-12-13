(ns aoc2020.aoc5
  (:require [clojure.string :as str]))

(defn parse-seat
  [in]
  (let [bin (-> in (str/replace #"[BR]" "1") (str/replace #"[FL]" "0"))]
    [(Long/parseLong (subs bin 0 7) 2) (Long/parseLong (subs bin 7) 2)]))

(def input (map parse-seat (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc20/aoc5.txt")))))

(defn seat-id [[row col]] (+ (* 8 row) col))

(defn p1 [in] (reduce max (map seat-id in)))

(defn p2 [in]
  (let [s (set (map seat-id in))]
    (for [i (range (reduce min s) (reduce max s))
          :when (not (s i))]
      i)))
