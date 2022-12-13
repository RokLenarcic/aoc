(ns aoc2021.aoc25
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [medley.core :as m]))

(def test-input "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>")
(def input (slurp "/Users/roklenarcic/aoc/aoc21/aoc25.txt"))

(defn parse-input [in]
  (inputs/map2d in #(when-not (= \. %) %)))

(defn step-righties [in cols]
  (m/map-kv-keys
    (fn [k v]
      (if (= v \>)
        (let [new-k (update k 1 #(mod (inc %) cols))]
          (if (in new-k) k new-k))
        k))
    in))

(defn step-downies [in rows]
  (m/map-kv-keys
    (fn [k v]
      (if (= v \v)
        (let [new-k (update k 0 #(mod (inc %) rows))]
          (if (in new-k) k new-k))
        k))
    in))

(defn step [rows cols in]
  (-> in
      (step-righties cols)
      (step-downies rows)))

(defn states [in]
  (let [[rows cols] (a/size* (inputs/char-array2d in identity))
        input (parse-input in)]
    (iterate (partial step rows cols) input)))

(defn p1 [in]
  (let [s (states in)]
    (m/find-first some? (map #(when (= %1 %2) (inc %3)) s (rest s) (range)))))
