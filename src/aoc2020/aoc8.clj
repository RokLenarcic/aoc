(ns aoc2020.aoc8
  (:require [aoc.computing :as comp]
            [medley.core :as m])
  (:import (java.util HashSet)))

(def test-input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")
(def input (slurp "/Users/roklenarcic/aoc/aoc20/aoc8.txt"))

(def ops
  {:nop (fn [state _] state)
   :acc (fn [state inst] (update state :acc + (:op1 inst)))
   :jmp (fn [state {:keys [op1]}] (update state :pc + op1 -1))})

(defn run-till-loop [prog]
  (let [seen? (HashSet.)]
    (comp/run-prog {:acc 0} prog ops #(or (comp/pc-invalid? %) (not (.add seen? (:pc %)))))))

(defn p1 [in] (:acc (run-till-loop (comp/parse-prog in))))
(defn p2 [in]
  (let [prog (comp/parse-prog in)
        runs (for [i (range (count prog))
                   :when (not= :acc (get-in prog [i :f]))]
               (run-till-loop (update-in prog [i :f] {:nop :jmp :jmp :nop})))]
    (:acc (m/find-first comp/pc-invalid? runs))))
