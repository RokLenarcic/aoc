(ns aoc2023.aoc23
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [aoc.inputs :as in]
            [clojure.string :as str])
  (:import (java.util LinkedList)))

(def test-input "#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc23.txt"))

(defn parse-input [in]
  (let [m (in/char-array2d in #(when (not= \# %) %))]
    {:m (a/->ArrayKd m a/dirs)
     :start [0 1]
     :end (let [[rows cols] (a/size* m)]
            [(dec rows) (- cols 2)])}))

(defn can-move? [p [p' v]]
  (let [dir (mapv - p' p)]
    (case v
      \. true
      \> (= dir [0 1])
      \< (= dir [0 -1])
      \v (= dir [1 0])
      \^ (= dir [-1 0]))))

(defn find-junction [arr final-point? prev-junction start]
  (loop [last prev-junction
         p start
         dist 1]
    (let [adjs (filter #(and (not= last (first %)) (can-move? p %)) (a/adjacent arr p))]
      (case (count adjs)
        0 (when (final-point? p) [p dist])
        1 (recur p (ffirst adjs) (inc dist))
        [p dist]))))

(defn adjacent-junctions [find-j arr junct]
  (keep #(find-j junct %) (keys (a/adjacent arr junct))))

(defn accumulate-junctions [^LinkedList q edges this-junction junctions]
  (reduce (fn [edges [p' dist]]
            (.offer q p')
            (update-in edges [this-junction p'] (fnil max 0) dist))
          edges
          junctions))

(defn junction-graph [arr start end]
  (let [find-j (partial find-junction arr #{start end})
        q (doto (LinkedList.) (.add start))]
    (reduce (fn [edges junct]
              (if (edges junct)
                edges
                (accumulate-junctions q edges junct (adjacent-junctions find-j arr junct))))
            {}
            (take-while some? (repeatedly #(.poll q))))))

(defn dfs-longest-search
  [junction-graph p seen end]
  (if (= p end)
    0
    (reduce-kv (fn [worst-cost p' cost]
                 (if (seen p')
                    worst-cost
                    (max worst-cost
                         (+ cost (dfs-longest-search junction-graph p' (conj seen p') end)))))
               Long/MIN_VALUE
               (junction-graph p))))

(defn p [in]
  (let [{:keys [m start end]} (parse-input in)]
    (dfs-longest-search (junction-graph m start end) start #{} end)))

(defn p1 [in] (p in))
(defn p2 [in] (p (str/replace in #"[<>^v]" ".")))
