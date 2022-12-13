(ns aoc2020.aoc24
  (:require [aoc.arrays :as a]
            [clojure.string :as str]
            [medley.core :as m]))

; hexagonal directions
(def dirs {"w" [0 -2] "e" [0 2] "se" [1 1] "sw" [1 -1] "ne" [-1 1] "nw" [-1 -1]})

(defn parse-input [in]
  (->> in str/trim str/split-lines
       (map #(re-seq #"se|ne|nw|sw|e|w" %))
       (map #(map dirs %))))

(def test-input (parse-input "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc24.txt")))

(defn go [start dirs] (reduce a/p-sum start dirs))
(defn init [in] (set (keys (m/filter-vals true? (reduce #(update %1 (go [0 0] %2) not) {} in)))))

(defn p1 [in] (->> in init vals count))

(defn round [state]
  (let [unexplored-adjacents (mapcat (partial a/adjacent* (vals dirs)) state)
        tiles-to-consider (distinct (concat unexplored-adjacents state))]
    (reduce
      (fn [acc p]
        (let [cnt (count (keep state (a/adjacent* (vals dirs) p)))]
          (if (state p)
            (if (or (zero? cnt) (< 2 cnt)) (disj acc p) acc)
            (if (= 2 cnt) (conj acc p) acc))))
      state
      tiles-to-consider)))

(defn p2 [in] (count (nth (iterate round (init in)) 100)))
