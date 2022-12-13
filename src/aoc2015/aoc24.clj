(ns aoc2015.aoc24
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]))

(def test-input [1 2 3 4 5 7 8 9 10 11])
(def input (mapv parse-long (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc15/aoc24.txt")))))

(defn candidates*
  [[item & more] n]
  (if (zero? n)
    ['()]
    (if more
      (concat (if (<= item n)
                (map #(cons item %) (candidates* more (unchecked-subtract n item)))
                [])
              (candidates* more n))
      (if (= item n) [[item]] []))))

(defn qe [group] (reduce * group))

(defn best-first-group [in group-n]
  (let [limit (quot (c/sum-of in) group-n)]
    (->> (candidates* in limit)
         (sort-by count)
         (partition-by count)
         first
         (map qe)
         sort
         first)))

(defn p1 [in] (best-first-group in 3))
(defn p2 [in] (best-first-group in 4))
