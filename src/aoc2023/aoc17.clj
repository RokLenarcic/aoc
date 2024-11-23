(ns aoc2023.aoc17
  (:require [aoc.arrays :as a]
            [aoc.inputs :as in]
            [ubergraph.alg :as alg]))

(def test-input "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc17.txt"))

(defn do-turn [[p dir _] turn-side]
  (let [new-dir (a/rot dir turn-side)]
    [(a/p-sum p new-dir) new-dir 1]))

(defn possibles2 [[p dir moves :as state]]
  [(when (> moves 3) (do-turn state :R))
   (when (> moves 3) (do-turn state :L))
   (when (< moves 10)
     [(a/p-sum p dir) dir (inc moves)])])

(defn possibles [[p dir moves :as state]]
  [(do-turn state :R)
   (do-turn state :L)
   (when (< moves 3)
     [(a/p-sum p dir) dir (inc moves)])])

(defn next-moves "Generate next moves, skipping ones that go out of scope"
  [costs possibles state]
  (keep
    #(when-let [cost (and % (get-in costs (first %)))]
       {:dest % :weight cost})
    (possibles state)))

(defn p "Search the space of tuples [[row col] last-direction num-moves-in-direction]"
  [in possibles-fn]
  (let [costs (in/char-array2d in (comp parse-long str))
        goal (mapv dec (a/size* costs))]
    (:cost
      (alg/shortest-path
        (partial next-moves costs possibles-fn)
        {:cost-attr :weight
         :start-nodes [[[0 0] [0 1] 0]
                       [[0 0] [1 0] 0]]
         :end-node? #(= (first %) goal)}))))

(defn p1 [in] (p in possibles))
(defn p2 [in] (p in possibles2))
