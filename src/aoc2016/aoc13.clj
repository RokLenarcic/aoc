(ns aoc2016.aoc13
  (:require
    [aoc.arrays :as a]
    [aoc.colls :refer [memoize*]]
    [loom.graph :as g]
    [loom.alg :as alg]))

(def input 1350)

(def open?
  (memoize*
    (fn [a [x y]]
      (even? (Long/bitCount (+ a (* x x) (* 3 x) (* 2 x y) (* y y) y))))))

(defn g [in]
  (g/fly-graph :start [1 1]
               :successors (fn [p] (filter #(open? in %) (a/adjacent* a/dirs p)))))

(defn p1 [in]
  (dec (count (alg/shortest-path (g in) [1 1] [31 39]))))

(defn p2 [in]
  (alg/bf-traverse (g in) [1 1] {:when #(and (<= 0 (first %1))
                                             (<= 0 (second %1))
                                             (<= %3 50))}))

