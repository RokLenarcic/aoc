(ns aoc2020.aoc10
  (:require [clojure.string :as str]
            [memento.core :as mem]
            [memento.config :as memc]))

(defn parse-input [in] (->> in str/trim str/split-lines (map parse-long) vec))

(def test-input (parse-input "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc10.txt")))

(def count-arrangements
  (fn [start end sorted-adapters]
    (if (= start end)
      1
      (loop [ret 0 [adapter & more] sorted-adapters]
        (if (and adapter (<= adapter (+ start 3)))
          (recur (+ ret (count-arrangements adapter end more)) more)
          ret)))))

(mem/memo #'count-arrangements {memc/type memc/caffeine})

(defn p1 [in]
  (let [f (frequencies (map - (next (sort in)) (sort in)))]
    (* (inc (f 1)) (inc (f 3)))))

(defn p2 [in] (count-arrangements 0 (reduce max in) (sort in)))
