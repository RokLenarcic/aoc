(ns aoc2015.aoc25)

(def numseq (iterate #(mod (* % 252533) 33554393) 20151125))

(def input [2978 3083])

(defn triangle-coord
  "1-based"
  [row col]
  (let [row* (dec row)
        col* (dec col)
        diag (+ col* row*)]
    (+ col* 1 (quot (* diag (inc diag)) 2))))

(defn p1 [coords]
  (nth numseq (dec (apply triangle-coord coords))))
