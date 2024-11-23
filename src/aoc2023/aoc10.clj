(ns aoc2023.aoc10
  (:require [aoc.colls :as c]
            [aoc.inputs :as in]
            [aoc.arrays :as a]
            [clojure.string :as str]
            [loom.alg :as l.alg]))

(def test-input ["...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........." \F])
(def input [(slurp "/Users/roklenarcic/aoc/aoc23/aoc10.txt") \J])

(defn start-position [in]
  (mapv #(inc (* 3 %)) (some #(when (= \S (val %)) (key %)) (in/map2d in identity))))

(defn bigly-char [v]
  (case v
    \. [[\. \. \.] [\. \+ \.] [\. \. \.]]
    \| [[\. \# \.] [\. \+ \.] [\. \# \.]]
    \- [[\. \. \.] [\# \+ \#] [\. \. \.]]
    \F [[\. \. \.] [\. \+ \#] [\. \# \.]]
    \J [[\. \# \.] [\# \+ \.] [\. \. \.]]
    \L [[\. \# \.] [\. \+ \#] [\. \. \.]]
    \7 [[\. \. \.] [\# \+ \.] [\. \# \.]]))

(defn biglify-input [in s-char]
  (->> (str/split-lines (str/replace in \S s-char))
       (mapcat #(apply map concat (map bigly-char %)))
       (mapv #(apply str %))
       (str/join \newline)))

(defn rope-loop [start m] (l.alg/bf-traverse (a/array->graph m) start))

(defn p1 [[in s-char]]
  (let [m (a/->MapKd (in/map2d (biglify-input in s-char) #(when-not (= \. %) %))
                     a/dirs)]
    (/ (count (rope-loop (start-position in) m)) 6)))

(defn other-coordinates-graph [coords]
  (let [[_ [maxrow maxcol]] (a/bounds coords)
        other (for [row (range -1 (* 2 maxrow))
                    col (range -1 (* 2 maxcol))
                    :when (not (coords [row col]))]
                [row col])]
    (a/array->graph (a/->MapKd (zipmap other (repeat \.)) a/dirs))))

(defn insides [m start]
  (let [g (other-coordinates-graph (set (rope-loop start m)))]
    (->> (l.alg/connected-components g)
         (map set)
         (remove #(% [0 0]))
         (apply concat)
         distinct)))

(defn p2 [[in s-char]]
  (let [m (a/->MapKd (in/map2d (biglify-input in s-char) #(when-not (= \. %) %))
                     a/dirs)
        inside-coords (insides m (start-position in))]
    (c/count-matching #(= \+ %) (map #(a/val-at m %) inside-coords))))
