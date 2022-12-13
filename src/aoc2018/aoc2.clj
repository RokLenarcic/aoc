(ns aoc2018.aoc2
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "/Users/roklenarcic/aoc/aoc18/aoc2.txt")))

(defn p1 [in]
  (let [counts (map (comp set vals frequencies) in)]
    (* (count (keep #(% 3) counts))
       (count (keep #(% 2) counts)))))

(defn remove-diff-chars [x y]
  (filter some? (mapv #(when (= %1 %2) %1) x y)))

(defn p2 [in]
  (for [x in y in
        :let [res (remove-diff-chars x y)]
        :when (= (dec (count x)) (count res))]
    (apply str res)))
