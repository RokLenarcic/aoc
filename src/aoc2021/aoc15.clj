(ns aoc2021.aoc15
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [clojure.string :as str]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(def test-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc15.txt")))

(defn parse-input [in]
  (a/->ArrayKd (inputs/char-array2d in (comp parse-long str)) a/dirs))

(defn upgrade-v
  "Add 1 to vector values with mod 10"
  [v] (mapv #(if (= % 9) 1 (inc %)) v))

(defn blocks [vvec] (iterate #(mapv upgrade-v %) vvec))

(defn expand-input
  "5x5 tiles"
  [in]
  (let [b-seq (blocks (inputs/char-array2d in (comp parse-long str)))]
    (reduce
      (fn [acc i]
        (into acc (->> b-seq (drop i) (take 5) (apply mapv (comp vec concat)))))
      []
      (range 5))))

(defn array->graph
  [arr]
  (let [nodes (a/filter-arr arr any?)
        ug-nodes (map (fn [[p v]] [p {:v v}]) nodes)
        ug-edges (for [n (keys nodes)
                       adj (a/adjacent arr n)]
                   [n (key adj) (val adj)])]
    (apply u/digraph (concat ug-nodes ug-edges))))

(defn shortest-path-cost [arr]
  (let [g (array->graph arr)]
    (alg/cost-of-path (alg/shortest-path g [0 0] (mapv dec (a/dim arr)) :weight))))

(defn p1 [in] (shortest-path-cost (parse-input in)))
(defn p2 [in] (shortest-path-cost (a/->ArrayKd (expand-input in) a/dirs)))
