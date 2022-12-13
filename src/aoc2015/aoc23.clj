(ns aoc2015.aoc23
  (:require [aoc.computing :as comp]))

(def ops {:hlf (comp/unary-op #(quot % 2))
          :tpl (comp/unary-op #(* 3 %))
          :inc (comp/unary-op inc)
          :jmp (fn [state inst] (update state :pc + -1 (comp/resolve-op1 state inst)))
          :jie (comp/jump-op even?)
          :jio (comp/jump-op #(= 1 %))})

(def test-prog (comp/parse-prog "inc a\njio a, +2\ntpl a\ninc a"))
(def input (comp/parse-prog (slurp "/Users/roklenarcic/aoc/aoc15/aoc23.txt")))

(defn p1 [p] (:b (comp/run-prog {:a 0 :b 0} p ops comp/pc-invalid?)))
(defn p2 [p] (:b (comp/run-prog {:a 1 :b 0} p ops comp/pc-invalid?)))
