(ns aoc2018.aoc10
  (:require [aoc.inputs :as inputs]
            [aoc.colls :as c]
            [aoc.arrays :as a]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc10.txt")))

(defn parse-input [in]
  (->> (str/split-lines in)
       (map (partial inputs/parse-numbers true))
       (map (fn [[x y vx vy]]  [[y x] [vy vx]]))
       (into [])))

(defn moves [in]
  (iterate #(map (fn [[k v]] [(mapv + k v) v]) %) (parse-input in)))

(defn candidate?
  "Every point is adjacent to at least one other point?"
  [points-and-vels]
  (let [adjacents (partial a/adjacent* (concat a/dirs a/diagonal-dirs))
        points (into #{} (map first) points-and-vels)]
    (every? #(some points (adjacents %)) points)))

(defn p12 [in]
  (loop [[x & more] (moves in)
         i 0]
    (if (candidate? x)
      (do (println i)
          (a/print-2d-map (into {} x)
                          #(if % \X \space)))
      (recur more (inc i)))))
