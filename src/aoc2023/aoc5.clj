(ns aoc2023.aoc5
  (:require [aoc.inputs :as in])
  (:import (java.util TreeMap)))

(def test-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc5.txt"))

(defn ->map [ranges]
  (let [ret (TreeMap.)]
    (doseq [r (rest ranges)
            :let [[to from cnt] (in/parse-numbers r)]]
      (.putIfAbsent ret (dec from) 0)
      (.put ret (+ from cnt -1) (- to from)))
    ret))

(defn parse-input [in]
  (let [[seeds & more] (in/blocks in)]
    {:seeds (in/parse-numbers (first seeds))
     :mappings (mapv ->map more)}))

(defn sub-intervals
  [^TreeMap m [start end]]
  (loop [acc []
         i start
         [[j adj] & more] (.subMap m start true end true)]
    (if (and j (< j end))
      (recur (conj acc [(+ i adj) (+ j adj)]) (inc j) more)
      (let [adj (or (some-> (.ceilingEntry m i) val) 0)]
        (conj acc [(+ i adj) (+ end adj)])))))

(defn apply-mapping [intervals mapping] (mapcat (partial sub-intervals mapping) intervals))

(defn p [in seeds-init]
  (let [{:keys [seeds mappings]} (parse-input in)]
    (->> (reduce apply-mapping (seeds-init seeds) mappings)
         (transduce (map first) min Long/MAX_VALUE))))

(defn p1 [in] (p in #(map (fn [x] [x x]) %)))
(defn p2 [in] (p in #(->> % (partition-all 2) (map (fn [[start cnt]] [start (+ start cnt -1)])))))

