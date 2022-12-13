(ns aoc2015.aoc18
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as inputs]))

(defn parse-input [in]
  (a/->ArrayKd
    (inputs/char-array2d in #(when (= \# %) true))
    (into a/dirs a/diagonal-dirs)))

(def test-input (parse-input ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc15/aoc18.txt")))

(defn step [arr]
  (reduce
    (fn [arr* p]
      (a/update-val
        arr* p
        (fn [v]
          (let [n-adj (count (filter identity (vals (a/adjacent arr p))))]
            (if v (<= 2 n-adj 3) (= n-adj 3))))))
    arr
    (keys (a/filter-arr arr any?))))

(defn turn-on-corners [arr]
  (let [[maxx maxy] (a/dim arr)]
    (-> arr
        (a/update-val [0 0] (constantly true))
        (a/update-val [(dec maxx) 0] (constantly true))
        (a/update-val [0 (dec maxy)] (constantly true))
        (a/update-val [(dec maxx) (dec maxy)] (constantly true)))))

(defn p1 [in]
  (count (a/filter-arr (nth (iterate step in) 100) identity)))

(defn p2 [in]
  (count (a/filter-arr (nth (iterate (comp turn-on-corners step) (turn-on-corners in)) 100) identity)))
