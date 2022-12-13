(ns aoc2016.aoc18
  (:require [clojure.string :as str]))

(def trap-templates #{[\^ \. \.] [\. \. \^] [\. \^ \^] [\^ \^ \.]})

(def test-input ".^^.^.^^^^")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc16/aoc18.txt")))

(defn next-row [row]
  (mapv (fn [l c r] (if (trap-templates [l c r]) \^ \.))
        (cons \. row)
        row
        (next (conj row \.))))

(defn p [in rows]
  (->> (iterate next-row (vec in))
       (take rows)
       (mapcat identity)
       (filter #(= % \.))
       count))
