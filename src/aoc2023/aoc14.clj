(ns aoc2023.aoc14
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [aoc.inputs :as in]))

(def test-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc14.txt"))

(defn parse-input [in] (in/map2d in #(when (not= % \.) %)))

(defn roll-north [m]
  (->> (into (sorted-map) m)
       (reduce-kv (fn [{:keys [support ret]} [row col] v]
                    (let [new-row (if (= \O v)
                                    (inc (support col -1))
                                    row)]
                      ;; Roll rock up to a support, becomes new support for column
                      {:support (assoc support col new-row)
                       :ret (assoc ret [new-row col] v)}))
                  {:support {} :ret {}})
       :ret))

(defn one-cycle [m]
  (let [roll-and-rotate (comp #(a/rot-map % :R) roll-north)]
    (nth (iterate roll-and-rotate m) 4)))

(defn calc-load [m]
  (let [[_ [max-row _]] (a/bounds (keys m))]
    (c/sum-of (fn [[[row _col] v]]
                (if (= v \O) (inc (- max-row row)) 0))
              m)))

(defn p1 [in] (calc-load (roll-north (parse-input in))))

(defn p2 [in]
  (let [boards (iterate one-cycle (parse-input in))
        {:keys [prefix length]} (c/find-cycle-of-val boards)]
    (->> (+ prefix (mod (- 1000000000 prefix) length))
         (nth boards)
         calc-load)))
