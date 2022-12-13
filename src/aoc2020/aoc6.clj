(ns aoc2020.aoc6
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [clojure.set :as set]))

(def test-input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")
(def input (slurp "/Users/roklenarcic/aoc/aoc20/aoc6.txt"))

(defn answer [in combine-fn] (c/sum-of count (inputs/blocks in :block-fn combine-fn)))

(defn p1 [in] (answer in #(reduce into #{} %2)))
(defn p2 [in] (answer in #(reduce set/intersection (map set %2))))
