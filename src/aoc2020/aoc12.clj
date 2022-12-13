(ns aoc2020.aoc12
  (:require
    [aoc.arrays :as a]
    [clojure.string :as str]))

(defn parse-input [in]
  (->> in str/trim str/split-lines (map #(vector (first %) (parse-long (subs % 1))))))

(def test-input (parse-input "F10\nN3\nF7\nR90\nF11"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc12.txt")))

(defn rot-degress [dir side amt] (nth (iterate #(a/rot % side) dir) (quot amt 90)))

(defn move [[p dir] [c amt]]
  (case c
    \F [(a/p-sum p (map * (repeat amt) dir)) dir]
    (\R \L) [p (rot-degress dir c amt)]
    \N [(a/p-sum p (map * (repeat amt) [-1 0])) dir]
    \S [(a/p-sum p (map * (repeat amt) [1 0])) dir]
    \E [(a/p-sum p (map * (repeat amt) [0 1])) dir]
    \W [(a/p-sum p (map * (repeat amt) [0 -1])) dir]))

(defn move2 [[p way] [c amt]]
  (case c
    \F [(a/p-sum p (map * (repeat amt) way)) way]
    (\R \L) [p (rot-degress way c amt)]
    \N [p (a/p-sum way (map * (repeat amt) [-1 0]))]
    \S [p (a/p-sum way (map * (repeat amt) [1 0]))]
    \E [p (a/p-sum way (map * (repeat amt) [0 1]))]
    \W [p (a/p-sum way (map * (repeat amt) [0 -1]))]))

(defn p1 [in] (->> (reduce move [[0 0] [0 1]] in) first (map abs) (reduce +)))
(defn p2 [in] (->> (reduce move2 [[0 0] [-1 10]] in) first (map abs) (reduce +)))
