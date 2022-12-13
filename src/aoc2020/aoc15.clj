(ns aoc2020.aoc15
  (:require [medley.core :as m]))

(def input [1,0,18,10,19,6])

(defn ingest [in]
  (reductions
    (fn [[n seen-idxs] [i n']] [n' (assoc seen-idxs n i)])
    [(first in) {}]
    (m/indexed (rest in))))

(defn derive-sequence [init]
  (reductions
    (fn [[n seen-idxs] i]
      [(if-let [idx (seen-idxs n)]
         (- i idx) 0)
       (assoc seen-idxs n i)])
    init
    (range (count (second init)) Long/MAX_VALUE)))

(defn numbers [in]
  (let [prefix (ingest in)]
    (concat (butlast prefix) (derive-sequence (last prefix)))))

(defn p1 [in] (first (nth (numbers in) 2019)))
(defn p2 [in] (first (nth (numbers in) (dec 30000000))))
