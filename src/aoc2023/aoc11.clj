(ns aoc2023.aoc11
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [aoc.inputs :as in]))

(def test-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc11.txt"))

(defn adjust-dim [c dim adj-size]
  (let [size (inc ((second (a/bounds c)) dim))
        empty-dim? (fn [x] (not-any? #(= (% dim) x) c))
        steps (vec (reductions #(cond-> %1 (empty-dim? %2) inc) (range size)))]
    (mapv (fn [p] (update p dim #(+ % (* adj-size (steps %))))) c)))

(defn expand [in adj-size]
  (-> (keys (in/map2d in #(when (= \# %) %)))
      (adjust-dim 0 adj-size)
      (adjust-dim 1 adj-size)))

(defn min-distances [c] (c/sum-of (for [x c y c :when (neg? (compare x y))]
                                    (reduce + (map (comp abs -) x y)))))

(defn p1 [in] (min-distances (expand in 1)))
(defn p2 [in] (min-distances (expand in (dec 1000000))))
