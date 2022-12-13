(ns aoc2021.aoc11
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [clojure.string :as str]))

(def test-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc11.txt")))

(defn blink
  "Blink one"
  [arr p]
  (reduce
    (fn [arr pp] (a/update-val arr pp inc))
    (a/update-val arr p (constantly -10000000))
    (keys (a/adjacent arr p))))

(defn step
  "One step"
  [{:keys [arr blinks]}]
  (loop [uarr (a/update-all arr (fn [_ v] (inc v)))
         blinks blinks]
    (let [blinkers (a/filter-arr uarr #(< 9 %))]
      (if (seq blinkers)
        (recur (reduce blink uarr (keys blinkers))
               (+ blinks (count blinkers)))
        {:arr (a/update-all uarr (fn [_ v] (if (neg-int? v) 0 v))) :blinks blinks}))))

(defn parse-input [in]
  (a/->ArrayKd (inputs/char-array2d in (comp parse-long str)) (into a/dirs a/diagonal-dirs)))

(defn p1 [in]
  (:blinks (nth (iterate step {:arr (parse-input in) :blinks 0}) 100)))

(defn p2 [in]
  (->> (iterate step {:arr (parse-input in) :blinks 0})
       (keep-indexed (fn [idx val]
                       (when (->> val :arr :arr (every? (partial every? zero?)))
                         idx)))
       first))
