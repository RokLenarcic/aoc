(ns aoc2021.aoc24)

(def input (slurp "/Users/roklenarcic/aoc/aoc21/aoc24.txt"))

(def constraints
  "Input idx x = [input idx y + integer]"
  {7 [0 -7]
   6 [1 0]
   3 [2 -8]
   5 [4 -3]
   9 [8 5]
   12 [11 -6]
   13 [10 3]})

(defn numbers [num-so-far num-range depth]
  (if (= 14 depth)
    [num-so-far]
    (for [i num-range
          :let [[const-idx const-x] (constraints depth)]
          :when (or (nil? const-idx)
                    (= i (+ (num-so-far const-idx) const-x)))
          res (numbers (conj num-so-far i) num-range (inc depth))]
      res)))

(defn p1 [] (apply str (first (numbers [] (range 9 0 -1) 0))))
(defn p2 [] (apply str (first (numbers [] (range 1 10) 0))))
