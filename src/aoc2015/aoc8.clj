(ns aoc2015.aoc8
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "/Users/roklenarcic/aoc/aoc15/aoc8.txt")))
(defn unescape [s]
  (str/replace s #"\\\\|\\\"|\\x[0-9a-f][0-9a-f]" "?"))

(defn escape [s] (str/replace s #"\"|\\" "??"))

(defn p1 [in]
  (reduce
    (fn [acc s]
      (- (+ acc (count s)) (- (count (unescape s)) 2)))
    0
    in))

(defn p2 [in]
  (reduce
    (fn [acc s]
      (- (+ acc (count (escape s)) 2) (count s)))
    0
    in))
