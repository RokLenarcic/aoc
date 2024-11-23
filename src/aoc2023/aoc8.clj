(ns aoc2023.aoc8
  (:require [aoc.math :as math]
            [clojure.string :as str]))

(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc8.txt"))

(defn parse-input [in]
  (let [[dirs _ & graph] (str/split-lines in)
        fork (fn [s] (let [[x y z] (re-seq #"\w+" s)]
                       [x [y z]]))]
    {:dirs (seq dirs)
     :forks (into {} (map fork graph))}))

(defn cycle-size [{:keys [dirs forks]} state stop-state-pred]
  (->> (reductions #((forks %1) (if (= \R %2) 1 0)) state (cycle dirs))
       (take-while (complement stop-state-pred))
       count))

(defn p1 [in] (cycle-size (parse-input in) "AAA" #(= % "ZZZ")))

(defn p2 [in]
  (let [m (parse-input in)]
    (->> (keys (:forks m))
         (filter #(str/ends-with? % "A"))
         (mapv (fn [x] (cycle-size m x #(str/ends-with? % "Z"))))
         (reduce math/lcm))))
