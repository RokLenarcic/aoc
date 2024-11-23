(ns aoc2023.aoc16
  (:require [aoc.arrays :as a]
            [aoc.inputs :as in])
  (:import (java.util HashSet LinkedList)))

(def test-input ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc16.txt"))

(defn new-dirs [v dir]
  (case v
    \. [dir]
    \\ [(vec (reverse dir))]
    \/ [(mapv #(* -1 %) (reverse dir))]
    \| (if (zero? (first dir))
         [[-1 0] [1 0]]
         [dir])
    \- (if (zero? (second dir))
         [[0 -1] [0 1]]
         [dir])))

(defn bfs [in start]
  (let [q (doto (LinkedList.) (.add start))
        seen (HashSet.)]
    (while (not (.isEmpty q))
      (let [[p dir] (.removeFirst q)]
        (when-let [v (and (not (.contains seen [p dir])) (get-in in p))]
          (.add seen [p dir])
          (doseq [new-dir (new-dirs v dir)]
            (.offerLast q [(a/p-sum p new-dir) new-dir])))))
    (distinct (mapv first seen))))

(defn p [in init] (count (bfs in init)))

(defn p1 [in] (p (in/char-array2d in identity) [[0 0] [0 1]]))

(defn p2 [in]
  (let [in (in/char-array2d in identity)
        [rows cols] (a/size* in)
        inits (concat (mapcat (fn [i] [[[i 0] [0 1]] [[i (dec cols)] [0 -1]]]) (range rows))
                      (mapcat (fn [i] [[[0 i] [1 0]] [[(dec rows) i] [-1 0]]]) (range cols)))]
    (apply max (mapv #(p in %) inits))))
