(ns aoc2021.aoc13
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [clojure.string :as str]))

(def test-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc13.txt")))

(defn fold-idx
  "Find fold index"
  [i fold-idx]
  (- fold-idx (abs (- i fold-idx))))

(defn parse-block [idx b]
  (case idx
    0 (mapv #(mapv parse-long (str/split % #",")) b)
    1 (mapv #(rest (re-find #"fold along ([xy])=(\d+)" %)) b)))

(defn execute-fold [m [axis idx]]
  (distinct (mapv #(update % (if (= "x" axis) 0 1) fold-idx idx) m)))

(defn parse-input [in]
  (let [[points folds] (inputs/blocks in :block-fn parse-block)]
    {:points points
     :folds (->> folds (mapv vec) (mapv #(update % 1 parse-long)))}))

(defn fold-input [in]
  (let [{:keys [points folds]} (parse-input in)]
    (reductions execute-fold points folds)))

(defn p1 [in] (count (second (fold-input in))))

(defn p2 [in] (a/print-2d-map (zipmap (map reverse (last (fold-input in))) (repeat true))))
