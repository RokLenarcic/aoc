(ns aoc2018.aoc18
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]
            [aoc.arrays :as a]))

(def test-input ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.\n")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc18.txt")))

(defn parse-input [in]
  (a/->ArrayKd (inputs/char-array2d in identity)
               (into a/dirs a/diagonal-dirs)))

(defn update-p [arr p v]
  (let [{trees \| yards \# :or {trees 0 yards 0}} (frequencies (vals (a/adjacent arr p)))]
    (case v
      \# (if (and (<= 1 trees) (<= 1 yards)) \# \.)
      \. (if (<= 3 trees) \| \.)
      \| (if (<= 3 yards) \# \|))))

(defn step [arr] (a/update-all arr (partial update-p arr)))
(defn score [state] (* (count (a/find-val state \#))
                       (count (a/find-val state \|))))

(defn p1 [in] (score (nth (iterate step (parse-input in)) 10)))

(defn p2 [in]
  (loop [seen {}
         idx (int 1e09)
         state (parse-input in)]
    (if (zero? idx)
      (score state)
      (if-let [last-idx (seen state)]
        (recur {} (long (mod idx (- last-idx idx))) state)
        (recur (assoc seen state idx) (long (dec idx)) (step state))))))
