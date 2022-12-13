(ns aoc2020.aoc3
  (:require
    [aoc.arrays :as a]
    [aoc.colls :as c]
    [aoc.inputs :as inputs]))

(def input (inputs/char-array2d (slurp "/Users/roklenarcic/aoc/aoc20/aoc3.txt") identity))
(def test-input (inputs/char-array2d "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#" identity))

(defn count-trees [in slope]
  (let [cols (count (first in))
        locations (iterate #(update (a/p-sum % slope) 1 mod cols) [0 0])
        values (take-while some? (map #(get-in in %) locations))]
    (c/count-matching #(= % \#) values)))

(defn p1 [in] (count-trees in [1 3]))
(defn p2 [in] (reduce * (map #(count-trees in %) [[1 1] [1 3] [1 5] [1 7] [2 1]])))


