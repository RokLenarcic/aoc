(ns aoc2021.aoc20
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [clojure.string :as str]
            [medley.core :as m]))

(def dirs (vec (concat a/dirs a/diagonal-dirs)))
(defn parse-input [in]
  (zipmap
    [:alg :lights]
    (inputs/blocks in :block-fn (fn [i b]
                                  (case i
                                    0 (-> (first b) (str/replace \. \0) (str/replace \# \1))
                                    1 (inputs/map2d (str/join \newline b) {\. \0 \# \1}))))))

(def test-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###")
(def input (slurp "/Users/roklenarcic/aoc/aoc21/aoc20.txt"))

(defn active-on-map [m] (keys (m/filter-vals #(= \1 %) m)))

(defn convolve [{:keys [alg lights]} p default]
  (let [kernel (sort (cons p (a/adjacent* dirs p)))
        idx (Long/parseLong (apply str (map #(lights % default) kernel)) 2)]
    (nth alg idx)))

(defn step [{:keys [lights default alg] :as in} ]
  (let [p-of-interest (distinct (mapcat #(cons % (a/adjacent* dirs %)) (keys lights)))
        new-lights (reduce
                     (fn [acc p]
                       (assoc acc p (convolve in p default)))
                     lights
                     p-of-interest)]
    {:alg alg
     :lights new-lights
     :default (if (= \0 (first alg))
                default
                ({\1 \0 \0 \1} default))}))

(defn run-steps [in n]
  (-> (iterate step (assoc (parse-input in) :default \0))
      (nth n)
      :lights
      active-on-map
      count))

(defn p1 [in] (run-steps in 2))
(defn p2 [in] (run-steps in 50))

