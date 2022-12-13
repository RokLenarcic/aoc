(ns aoc2016.aoc15
  (:require [aoc.colls :as c]
            [clojure.string :as str]
            [medley.core :as m]))

(defn parse-disc [l]
  (let [[_ disc n start] (re-find #"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)." l)]
    (c/map-of (parse-long disc) (parse-long n) (parse-long start))))

(defn parse-input [in]
  (->> in str/trim
       str/split-lines
       (map parse-disc)))

(defn pass-disc [i {:keys [disc n start]}] (zero? (mod (+ i start disc) n)))

(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc16/aoc15.txt")))
(def test-input (parse-input "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."))
(defn tt-get-capsule [in]
  (m/find-first #(every? (partial pass-disc %) in) (range)))

(defn p1 [in] (tt-get-capsule in))

(defn p2 [in] (tt-get-capsule (cons {:disc (inc (reduce max (map :disc in))) :n 11 :start 0} in)))
