(ns aoc2018.aoc16
  (:require [aoc.colls :as c]
            [aoc.inputs :as inputs]
            [aoc2018.machine1 :as m1]
            [clojure.set :as set]
            [clojure.string :as str]))

(def test-input "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]")
(def input (inputs/blocks (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc16.txt"))))

(defn parse-sample [[before inst after]]
  (let [before (zipmap (range 4)
                       (read-string (subs before 8)))
        after (zipmap (range 4)
                      (read-string (subs after 7)))]
    {:before before :after after :inst (m1/parse-inst inst)}))

(defn guess-inst [{:keys [before after inst]}]
  (for [opcode m1/opcodes
        :when (= after (m1/oper before (assoc inst :f opcode)))]
    opcode))

(defn p1 [in]
  (let [samples (map parse-sample (butlast in))]
    (c/count-matching #(<= 3 (count (guess-inst %))) samples)))

(defn guess-opcodes* [x->opcode opcode->x]
  (if (= {} x->opcode)
    opcode->x
    (let [[x opcodes] (first x->opcode)]
      (some #(guess-opcodes* (dissoc x->opcode x) (assoc opcode->x % x))
            (remove opcode->x opcodes)))))

(defn guess-opcodes [samples]
  (let [valid (apply merge-with
                set/intersection
                (map (fn [sample] {(-> sample :inst :f) (set (guess-inst sample))}) samples))]
    (set/map-invert (guess-opcodes* valid {}))))

(defn p2 [in]
  (let [test-prog (map m1/parse-inst (last in))
        opcode-map (guess-opcodes (map parse-sample (butlast in)))]
    (reduce
      m1/oper
      {0 0 1 0 2 0 3 0}
      (mapv #(update % :f opcode-map) test-prog))))
