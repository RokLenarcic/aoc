(ns aoc2020.aoc11
  (:require
    [aoc.arrays :as a]
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [medley.core :as m]))

(defn parse-input [in]
  (a/->ArrayKd (inputs/char-array2d in identity) (into a/dirs a/diagonal-dirs)))

(def test-input (parse-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc11.txt")))

(defn seat? [v] (#{\L \#} v))
(defn first-seat? [arr points]
  (some #(let [v (a/val-at arr %)] (when (seat? v) v)) points))

(defn round [arr]
  (a/update-all
    arr
    (fn [p v]
      (case v
        \# (if (< (c/count-matching #(= \# %) (vals (a/adjacent arr p))) 4) \# \L)
        \L (if (not-any? #(= \# %) (vals (a/adjacent arr p))) \# \L)
        v))))

(defn occupied-seats [in round-fn]
  (let [i (iterate round-fn in)
        sol (m/find-first some? (map #(when (= %1 %2) %1) i (rest i)))]
    (count (a/filter-arr sol #(= \# %)))))

(defn p1 [in] (occupied-seats in round))

(defn round2 [arr]
  (a/update-all
    arr
    (fn [p v]
      (if (seat? v)
        (let [a (a/as-nested-vec arr)
              visible (keep (partial first-seat? arr) (a/fan-out a (:dirs arr) p))]
          (case v
            \# (if (< (c/count-matching #(= \# %) visible) 5) \# \L)
            \L (if (not-any? #(= \# %) visible) \# \L)
            v))
        v))))

(defn p2 [in] (occupied-seats in round2))
