(ns aoc2018.aoc22
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [clojure.string :as str]
            [ubergraph.alg :as alg]
            [aoc.colls :as c]))

(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc22.txt")))

(declare erosion-level)

(defn geologic-index [p {:keys [target] :as params}]
  (cond
    (= p [0 0]) 0
    (= p target) 0
    (zero? (p 0)) (* 48271 (p 1))
    (zero? (p 1)) (* 16807 (p 0))
    :else (* (erosion-level (a/p-sum p [-1 0]) params)
             (erosion-level (a/p-sum p [0 -1]) params))))

(def erosion-level
  (c/memoize*
    (fn [p {:keys [depth] :as params}]
      (mod (+ (geologic-index p params) depth) 20183))))

(defn type-of [p params]
  (mod (erosion-level p params) 3))

(defn parse-input [in]
  (let [[[depth] [target-x target-y]] (mapv inputs/parse-numbers (str/split-lines in))]
    {:depth depth
     :target [target-x target-y]}))

(defn p1 [in]
  (let [{:keys [target] :as params} (parse-input in)
        [target-x target-y] target]
    (reduce +
            (for [x (range (inc target-x))
                  y (range (inc target-y))]
              (type-of [x y] params)))))

(def equipments {0 #{:climb :torch}
                 1 #{:climb :none}
                 2 #{:torch :none}})

(defn successors [params [p equip]]
  (let [this-terrain (type-of p params)
        equipment-switch {:dest [p (first (disj (equipments this-terrain) equip))] :cost 7}]
    (cons equipment-switch
          (keep #(let [allowed (equipments (type-of % params))]
                   (when (allowed equip)
                     {:dest [% equip] :cost 1}))
                (filter #(every? nat-int? %) (a/adjacent* a/dirs p))))))

(defn p2 [in]
  (let [{:keys [target] :as params} (parse-input in)]
    (alg/cost-of-path (alg/shortest-path (partial successors params) [[0 0] :torch] [target :torch] :cost))))
