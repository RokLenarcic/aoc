(ns aoc2016.aoc9
  (:require [clojure.string :as str])
  (:import (java.util.regex Matcher)))

(def input (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc16/aoc9.txt"))))

(defn calculate-length [in nested?]
  (let [^Matcher m (re-matcher #"\((\d+)x(\d+)\)" in)]
    (loop [i 0 size 0]
      (if (.find m i)
        (let [selection-size (parse-long (.group m 1))
              selection-multiplier (parse-long (.group m 2))
              end-of-expansion (+ (.end m) selection-size)
              selected (subs in (.end m) end-of-expansion)]
          (recur (long end-of-expansion)
                 (long (+ size (- (.start m) i) (* selection-multiplier
                                                   (if nested?
                                                     (calculate-length selected nested?)
                                                     selection-size))))))
        (+ size (- (count in) i))))))

(defn p1 [in] (reduce + (map #(calculate-length % false) in)))
(defn p2 [in] (reduce + (map #(calculate-length % true) in)))
