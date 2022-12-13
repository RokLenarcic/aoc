(ns aoc2020.aoc9
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]
    [medley.core :as m]))

(defn preamble-table [preamble]
  (map
    (fn [i]
      [(preamble i)
       (reduce
         conj
         []
         (map #(+ (preamble i) %) (drop (inc i) preamble)))])
    (range (count preamble))))

(defn add-number
  "Drop first row, drop first col in each remaining row, add new col with
  new number at the end of each q, add another empty q"
  [table n]
  (conj
    (mapv
      (fn [[x q]] [x (conj q (+ x n))])
      (rest table))
    [n []]))

(defn has-number? [table n] (m/find-first #(= n %) (mapcat second table)))

(defn parse-input [in] (->> in str/trim str/split-lines (map parse-long)))
(def test-input (parse-input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc9.txt")))

(defn p1 [in preamble-size]
  (let [table (preamble-table (vec (take preamble-size in)))]
    (reduce
      (fn [table n]
        (if (has-number? table n)
          (add-number table n)
          (reduced n)))
      table
      (drop preamble-size in))))

(defn p2 [in n]
  (let [ns (vec in)]
    (some
      #(some
         (fn [j]
           (let [vv (subvec ns % (inc j))]
             (when (= n (c/sum-of vv))
               (+ (reduce min vv) (reduce max vv)))))
         (range (inc %) (count ns)))
      (range (count ns)))))
