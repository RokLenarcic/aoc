(ns aoc2015.aoc12
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [medley.core :as m]))

(def input (json/read-str (slurp "/Users/roklenarcic/aoc/aoc15/aoc12.txt") :key-fn keyword))

(defn p1 [in]
  (let [a (atom 0)]
    (walk/postwalk #(do (when (number? %) (swap! a + %)) %) in)
    @a))

(defn red? [m] (and (map? m) (m/find-first #(= "red" %) (vals m))))

(defn p2 [in]
  (let [a (atom 0)]
    (walk/prewalk #(when-not (red? %) (when (number? %) (swap! a + %)) %) in)
    @a))
