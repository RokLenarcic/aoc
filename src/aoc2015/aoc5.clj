(ns aoc2015.aoc5
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]))

(def input (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc15/aoc5.txt"))))
(defn nice? [s]
  (and (<= 3 (count (re-seq #"[aeiou]" s)))
       (re-find #"(.)\1" s)
       (nil? (re-find #"ab|cd|pq|xy" s))))

(defn nice?* [s]
  (and (re-find #"(..).*\1" s)
       (re-find #"(.).\1" s)))

(defn p1 [in] (c/count-matching nice? in))
(defn p2 [in] (c/count-matching nice?* in))
