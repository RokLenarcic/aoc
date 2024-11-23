(ns aoc2023.aoc15
  (:require [aoc.colls :as c]
            [flatland.ordered.map :refer [ordered-map]]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc15.txt"))

(defn parse-input [in] "Returns coll of [whole string, label, value]"
  (re-seq #"(\w+)(?:=(\d+)|-)?" (str/trim in)))

(defn hash-string [s] (reduce #(mod (* (+ %1 (int %2)) 17) 256) 0 s))

(defn p1 [in] (c/sum-of (comp hash-string first) (parse-input in)))

(defn update-lens "If v is nil then remove lens else upsert lens"
  [bucket label v]
  (if v
    (assoc (or bucket (ordered-map)) label (parse-long v))
    (dissoc bucket label)))

(defn apply-expr [m [_ label v]] (update m (hash-string label) update-lens label v))

(defn focusing-power [[i [label v]]]
  (* v (+ 1 i) (+ 1 (hash-string label))))

(defn p2 [in]
  (->> (vals (reduce apply-expr {} (parse-input in)))
       (mapcat m/indexed)
       (c/sum-of focusing-power)))
