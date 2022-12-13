(ns aoc2015.aoc20
  (:require
    [medley.core :as m]))

(def input 33100000)

(defn p1 [in]
  (let [len (quot in 10)
        a (int-array len)]
    (loop [n 1]
      (let [gift (int (* 10 n))]
        (run! #(aset a % (+ (aget a %) gift)) (range n len n))
        (if (<= in (aget a n)) n (recur (unchecked-inc n)))))))

(defn p2 [in]
  (let [len (quot in 11)
        a (int-array len)]
    (loop [n 1]
      (let [gift (int (* 11 n))]
        (run! #(aset a % (+ (aget a %) gift)) (range n (min len (inc (* 50 n))) n))
        (if (<= in (aget a n)) n (recur (unchecked-inc n)))))))
