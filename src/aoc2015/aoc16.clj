(ns aoc2015.aoc16
  (:require [clojure.string :as str]
            [medley.core :as m]))

(def findings "children: 3\ncats: 7\nsamoyeds: 2\npomeranians: 3\nakitas: 0\nvizslas: 0\ngoldfish: 5\ntrees: 3\ncars: 2\nperfumes: 1")
(defn parse-findings [in]
  (->> in str/trim str/split-lines (map #(let [[_ k v] (re-find #"^(\w+): (\d+)" %)]
                                           [k (parse-long v)]))
       (into {})))

(defn parse-sue [l]
  (let [[_ i clauses] (re-find #"^Sue (\d+): (.*)" l)]
    (reduce
      (fn [acc [_ k v]]
        (assoc-in acc [i k] (parse-long v)))
      {}
      (re-seq #"(\w+): (\d+)" clauses))))

(def input (->> (slurp "/Users/roklenarcic/aoc/aoc15/aoc16.txt")
                str/trim str/split-lines
                (map parse-sue)
                (reduce merge)))

(defn p1 [in]
  (let [f (parse-findings findings)]
    (m/filter-vals
      (fn [props]
        (every? (fn [[k v]] (= v (f k))) props))
      in)))

(defn p2 [in]
  (let [f (parse-findings findings)]
    (m/filter-vals
      (fn [props]
        (every? (fn [[k v]]
                  (case k
                    ("cats" "trees") (> v (f k))
                    ("pomeranians" "goldfish") (< v (f k))
                    (= v (f k)))) props))
      in)))
