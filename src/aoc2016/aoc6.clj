(ns aoc2016.aoc6
  (:require
    [aoc.arrays :as a]
    [clojure.string :as str]))

(def test-input "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar")
(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc6.txt"))

(defn select-chars-by-freq [in select-fn]
  (->> (str/split-lines (str/trim in))
       a/rows-to-cols
       (map frequencies)
       (map select-fn)
       (map first)
       (apply str)))

(defn p1 [in] (select-chars-by-freq in #(apply max-key second %)))
(defn p2 [in] (select-chars-by-freq in #(apply min-key second %)))
