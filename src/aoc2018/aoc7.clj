(ns aoc2018.aoc7
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc7.txt")))

(defn parse-input [in]
  (map #(rest (re-find #"Step (\w+).* step (\w+)" %)) (str/split-lines in)))

(defn prerequisite-map [pairs]
  (update-vals
    (group-by second pairs)
    #(into #{} (map first) %)))

(defn duration [x] (+ 61 (- (int (first x)) (int \A))))

(defn next-state
  "Returns next state that can be done"
  [unfinished-states candidate-states prerequisites]
  (some #(when-not (seq (set/intersection (prerequisites %) (set unfinished-states))) %)
        candidate-states))

(defn order-states [states prerequisites workers]
  (loop [t 0 times {}]
    (if (= (count times) (count states))
      times
      (let [done (keys (m/filter-vals #(<= % t) times))]
        (if-let [s (and (< (- (count times) (count done)) workers)
                        (next-state (apply disj states done)
                                    (apply disj states (keys times))
                                    prerequisites))]
          (recur t (assoc times s (+ t (duration s))))
          (recur (inc t) times))))))

(defn solve [in workers]
  (let [pairs (parse-input in)
        prerequisites (prerequisite-map pairs)
        all-states (into (sorted-set) (flatten pairs))]
    (order-states all-states prerequisites workers)))

(defn p1 [in] (apply str (map first (sort-by val (solve in 1)))))

(defn p2 [in] (reduce max (vals (solve in 5))))
