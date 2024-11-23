(ns aoc2023.aoc2
  (:require [clojure.string :as str]
            [medley.core :as m]))

(def test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc2.txt"))

(defn parse-hand [s]
  (->> (re-seq #"(\d+) (red|green|blue)" s)
       (mapv (fn [[_ n type]] [(keyword type) (parse-long n)]))
       (into {})))

(defn parse-game [s]
  (let [[_ id] (re-find #"Game (\d+)" s)]
    [(parse-long id) (mapv parse-hand (str/split s #";"))]))

(defn parse-input [in] (into {} (mapv parse-game (str/split-lines in))))

(defn valid? [hand] (and (<= (:blue hand 0) 14) (<= (:green hand 0) 13) (<= (:red hand 0) 12)))
(defn minimum-hand [hands] (apply merge-with max hands))
(defn power [hand] (transduce (map #(hand % 1)) * [:blue :green :red]))

(defn p1 [in]
  (->> (parse-input in)
       (m/filter-vals #(every? valid? %))
       (keys)
       (reduce +)))

(defn p2 [in] (->> (parse-input in) vals (map minimum-hand) (map power) (reduce +)))
