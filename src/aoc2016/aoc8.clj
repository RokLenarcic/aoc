(ns aoc2016.aoc8
  (:require
    [aoc.inputs :as inputs]
    [aoc.arrays :as a]
    [clojure.string :as str]
    [medley.core :as m]))

(def rows 6)
(def cols 50)

(defn new-a []
  (vec (repeat rows (vec (repeat cols nil)))))

(defn rect [arr x y]
  (reduce
    #(assoc-in %1 %2 \#)
    arr
    (for [row (range y)
          col (range x)]
      [row col])))

(defn rot-col [arr col n]
  (reduce
    (fn [arr [i v]]
      (assoc-in arr [i col] v))
    arr
    (->> (range (count arr))
         (map #(get-in arr [% col]))
         cycle
         (drop (- rows n))
         (take rows)
         m/indexed)))

(defn rot-row [arr row n]
  (->> (arr row)
       cycle
       (drop (- cols n))
       (take cols)
       vec
       (assoc arr row)))

(defn parse-instruction [l]
  (inputs/reg-parse l
    [_ x y] #"rect (\d+)x(\d+)" #(rect % (parse-long x) (parse-long y))
    [_ row n] #"rotate row y=(\d+) by (\d+)" #(rot-row % (parse-long row) (parse-long n))
    [_ col n] #"rotate column x=(\d+) by (\d+)" #(rot-col % (parse-long col) (parse-long n))))

(def input (map parse-instruction (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc16/aoc8.txt")))))

(defn p1 [in]
  (reduce +
          (for [row (reduce (fn [arr f] (f arr)) (new-a) in)
                col row
                :when col]
            1)))

(defn p2 [in]
  (a/print-2d (reduce (fn [arr f] (f arr)) (new-a) in)))
