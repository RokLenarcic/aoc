(ns aoc2021.aoc3
  (:require
    [aoc.inputs :as inputs]
    [clojure.string :as str]))

(defn parse-input [in] (-> in (inputs/char-array2d identity)))
(def test-input (parse-input "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21/aoc3.txt")))

(defn most-common-bit [arr i]
  (let [m (frequencies (map #(get % i) arr))]
    (if (<= (m \0) (m \1)) \1 \0)))

(defn least-common-bit [arr i]
  (let [m (frequencies (map #(get % i) arr))]
    (if (<= (m \0) (m \1)) \0 \1)))

(defn to-num [arr] (-> arr str/join (Long/parseLong 2)))
(defn filter-by-bits [arr bit-fn]
  (reduce
    (fn [all-items i]
      (if (= 1 (count all-items))
        (reduced (first all-items))
        (filter #(= (bit-fn all-items i) (% i)) all-items)))
    arr
    (range)))

(defn p1 [in]
  (let [n (count (first in))]
    (* (to-num (map #(most-common-bit in %) (range n)))
       (to-num (map #(least-common-bit in %) (range n))))))

(defn p2 [in] (* (to-num (filter-by-bits in most-common-bit))
                 (to-num (filter-by-bits in least-common-bit))))
