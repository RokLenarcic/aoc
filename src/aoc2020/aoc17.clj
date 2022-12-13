(ns aoc2020.aoc17
  (:require
    [aoc.arrays :as a]
    [aoc.colls :as c]
    [aoc.inputs :as inputs]))

(def test-input ".#.\n..#\n###")
(def input (slurp "/Users/roklenarcic/aoc/aoc20/aoc17.txt"))

(defn parse-input [in] (->> (inputs/map2d in #(when (= \# %) true)) keys set))

(defn pad-to-dim [state dim]
  (loop [s state]
    (if (= (count (first s)) dim)
      s
      (recur (set (map #(conj % 0) s))))))

(defn round [neigh-fn state]
  (let [relevant-p (into state (mapcat neigh-fn state))
        enabled? #(if (state %)
                    (<= 2 (c/count-matching state (neigh-fn %)) 3)
                    (= 3 (c/count-matching state (neigh-fn %))))]
    (into #{} (filter enabled?) relevant-p)))

(defn result [in dims]
  (let [state (pad-to-dim (parse-input in) dims)
        neigh-fn (partial a/adjacent* (a/dirs-dim dims))]
    (count (nth (iterate (partial round neigh-fn) state) 6))))

(defn p1 [in] (result in 3))
(defn p2 [in] (result in 4))
