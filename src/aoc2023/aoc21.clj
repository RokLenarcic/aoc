(ns aoc2023.aoc21
  (:require [aoc.arrays :as a]
            [aoc.inputs :as in]))

(def test-input "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n...........")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc21.txt"))

(defn parse-input [in] (in/map2d in #(when-not (= \# %) %)))
(defn size [in] (let [[_ [max-row]] (a/bounds (keys (parse-input in)))]
                  (inc max-row)))
(defn start-node [in] (ffirst (in/map2d in #(when (= \S %) %))))

(defn p [points m mod-n]
  (cons
    points
    (lazy-seq
      (p (into #{} (comp (mapcat #(mapv a/p-sum (repeat %) a/dirs))
                         (filter #(contains? m [(mod (% 0) mod-n) (mod (% 1) mod-n) ])))
               points)
         m mod-n))))

(defn p1 [in]
  (let [m (parse-input in)]
    (count (nth (p [(start-node in)] m (size in)) 64))))

(defn p2 [in]
  (let [m (parse-input in)
        m-size (size in)]
    (->> (p [(start-node in)] m m-size)
         (keep-indexed (fn [idx cnt]
                         (when (= (mod idx m-size)
                                  (mod 26501365 m-size))
                           [idx cnt])))
         (take 4)
         ;; use wolfram to find the quadratic function for x,y pairs
         )))
