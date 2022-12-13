(ns aoc2020.aoc22
  (:require [aoc.inputs :as inputs]
            [memento.core :as mem]
            [memento.config :as memc])
  (:import (clojure.lang PersistentQueue)
           (java.util HashSet)))


(defn parse-input [in] (inputs/blocks in :item-fn parse-long :block-fn (fn [_ l] (next l))))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc22.txt")))
(def test-input (parse-input "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10"))

(defn rate [deck] (reduce + (map * (reverse deck) (next (range)))))

(defn take-q
  "Drop q head and take n"
  [q n]
  (into PersistentQueue/EMPTY (take n (next q))))

(defn combat [recursive? d1 d2]
  (let [seen? (HashSet.)]
    (loop [deck1 d1 deck2 d2]
      (cond
        (not (.add seen? [deck1 deck2])) [:p1 deck1]
        (empty? deck1) [:p2 deck2]
        (empty? deck2) [:p1 deck1]
        :else (let [c1 (peek deck1) c2 (peek deck2)
                    winner (if (and recursive? (< c1 (count deck1)) (< c2 (count deck2)))
                             (first (combat true (take-q deck1 c1) (take-q deck2 c2)))
                             (if (< c1 c2) :p2 :p1))]
                (if (= :p1 winner)
                  (recur (-> deck1 pop (conj c1 c2)) (pop deck2))
                  (recur (pop deck1) (-> deck2 pop (conj c2 c1)))))))))

(mem/memo #'combat {memc/type memc/caffeine})

(defn rate-winner [in recursive?]
  (rate (second (apply combat recursive? (map #(into PersistentQueue/EMPTY %) in)))))

(defn p1 [in] (rate-winner in false))
(defn p2 [in] (rate-winner in true))
