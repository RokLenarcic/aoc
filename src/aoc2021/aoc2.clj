(ns aoc2021.aoc2
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as inputs]
    [clojure.string :as str]))



(defn parse-command [l]
  (inputs/reg-parse l
    [_ units] #"forward (\d+)" [0 (parse-long units)]
    [_ units] #"up (\d+)" [(- (parse-long units)) 0]
    [_ units] #"down (\d+)" [(parse-long units) 0]))

(defn parse-input [in] (->> in str/trim str/split-lines (map parse-command)))

(def test-input (parse-input "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21/aoc2.txt")))

(defn p1 [in] (reduce * (reduce a/p-sum in)))
(defn p2 [in]
  (p1 (map (fn [[aim _] [_ fwd]] [(* aim fwd) fwd]) (reductions a/p-sum in) in)))
