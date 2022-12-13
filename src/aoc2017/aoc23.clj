(ns aoc2017.aoc23
  (:require [aoc.computing :as c]))

(def input (c/parse-prog (slurp "/Users/roklenarcic/aoc/aoc17/aoc23.txt")))
(def initial-state (zipmap [:a :b :c :d :e :f :g :h] (repeat 0)))

(defn p1 [in]
  (let [invocations (atom 0)
        opcodes {:set (c/bin-op (fn [_ v] v))
                 :sub (c/bin-op -)
                 :mul (comp #(do (swap! invocations inc) %) (c/bin-op *))
                 :jnz (c/jump-op #(not= 0 %))}]
    (c/run-prog initial-state in opcodes c/pc-invalid?)
    @invocations))

(defn prime? [n]
  (loop [i 2]
    (if (< i n)
      (if (zero? (mod n i)) false (recur (inc i)))
      true)))

(defn p2 [in]
  (count (remove prime? (range 108100 (inc 125100) 17))))

