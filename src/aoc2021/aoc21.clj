(ns aoc2021.aoc21
  (:require [aoc.colls :refer [memoize*]]))

(def test-input [4 8])
(def input [1 2])

(defn d3-deterministic [dice] [(* 3 dice) (+ dice 3)])

(defn rigged-game [p1-pos p2-pos]
  (loop [curr-player p1-pos
         curr-player-score 0
         next-player p2-pos
         next-player-score 0
         dice 2
         rolls 0]
    (let [[roll new-dice] (d3-deterministic dice)
          new-pos (inc (mod (dec (+ curr-player roll)) 10))
          new-score (+ new-pos curr-player-score)]
      (if (>= new-score 1000)
        [next-player-score (+ 3 rolls)]
        (recur next-player next-player-score new-pos new-score new-dice (+ 3 rolls))))))

(defn p1 [players] (reduce * (apply rigged-game players)))

(def roll-universes {3 1 4 3 5 6 6 7 7 6 8 3 9 1})

(def add-roll-to-player
  (memoize* (fn [max-score state roll player-idx]
              (let [[pos score] (state player-idx)
                    new-pos (inc (mod (dec (+ pos roll)) 10))
                    new-score (+ new-pos score)
                    new-state (assoc state player-idx [new-pos new-score])]
                (if (>= new-score max-score)
                  (update [0 0] player-idx inc)
                  (reduce-kv
                    (fn [acc roll times]
                      (->> (add-roll-to-player max-score new-state roll (mod (inc player-idx) 2))
                           (mapv #(* times %))
                           (mapv + acc)))
                    [0 0]
                    roll-universes))))))

(defn p2 [[p1 p2]]
  (reduce-kv
    (fn [acc roll times]
      (->> (add-roll-to-player 21 [[p1 0] [p2 0]] roll 0)
           (mapv #(* times %))
           (mapv + acc)))
    [0 0]
    roll-universes))

