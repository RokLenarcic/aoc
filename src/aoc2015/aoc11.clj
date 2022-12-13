(ns aoc2015.aoc11
  (:require [medley.core :as m]))

(defn valid? [s]
  (and (re-find #"abc|bcd|cde|def|efg|fgh|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz" s)
       (not (re-find #"[iol]" s))
       (let [pairs (into #{} (map first (re-seq #"(.)\1" s)))]
         (>= (count pairs) 2))))

(defn inc-char [^Character c]
  (char (inc (int c))))

(defn inc-password [[v & more]]
  (if more
    (let [tail (inc-password more)]
      (if (= (first tail) \{)
        (->> (rest tail) (cons \a) (cons (inc-char v)))
        (cons v tail)))
    [(inc-char v)]))

(defn p1 [in]
  (->> (next (iterate inc-password in))
       (map #(apply str %))
       (m/find-first valid?)))
