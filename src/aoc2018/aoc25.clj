(ns aoc2018.aoc25
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(def test-input "1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc25.txt")))

(defn dist [x y] (transduce (map abs) + (map - x y)))

(defn parse-input [in] (first (inputs/blocks in :item-fn #(inputs/parse-numbers true %))))
(defn nodes->edges [nodes]
  (for [n nodes
        n1 nodes
        :when (and (not= n n1) (<= (dist n n1) 3))]
    [n n1]))

(defn p1 [in]
  (let [nodes (parse-input in)
        edges (nodes->edges nodes)]
    (count (alg/connected-components (apply u/graph (concat nodes edges))))))
