(ns aoc2023.aoc1
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc1.txt"))
(def test-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")
(def test-input2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(def digits {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9})
(def values (merge digits {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9}))

(defn find-first [s candidates]
  (values (first (sort-by #(or (str/index-of s %) Long/MAX_VALUE) candidates))))

(defn find-last [s candidates]
  (values (last (sort-by #(or (str/last-index-of s %) Long/MIN_VALUE) candidates))))

(defn p [in candidates]
  (->> in str/split-lines
       (c/sum-of #(+ (* 10 (find-first % candidates))
                     (find-last % candidates)))))

(defn p1 [in] (p in (keys digits)))
(defn p2 [in] (p in (keys values)))
