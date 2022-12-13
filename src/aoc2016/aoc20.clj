(ns aoc2016.aoc20
  (:require [clojure.string :as str]))

(def test-input "5-8\n0-2\n4-7")
(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc20.txt"))

(defn combine-intervals
  "Combine intervals"
  [intervals end-inclusive?]
  (loop [points []
         prev-end (long Long/MIN_VALUE)
         [[start end] & more] (sort-by first intervals)]
    (if start
      (if (< (cond-> prev-end end-inclusive? inc) start)
        (recur (conj points prev-end start) (long end) more)
        (recur points (long (max prev-end end)) more))
      (vec (next (conj points prev-end))))))

(defn parse-input [in] (->> in str/trim str/split-lines (map #(mapv parse-long (str/split % #"-")))))

(defn p1 [in]
  (let [intervals (parse-input in)]
    (dec (nth (combine-intervals intervals true) 2))))

(defn p2 [in]
  (let [intervals (parse-input in)
        pairs (partition 2 (combine-intervals intervals true))
        num-blocked (reduce (fn [blocked [start end]] (+ blocked (- end start -1)))
                            0
                            pairs)]
    (- (Math/pow 2 32) num-blocked)))
