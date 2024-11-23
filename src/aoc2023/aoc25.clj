(ns aoc2023.aoc25
  (:require [aoc.arrays :as a]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as str]
            [loom.alg :as lalg]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(def test-input "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc25.txt"))

(defn parse-input [in]
  (->> (str/split-lines in)
       (map #(re-seq #"\w+" %))
       (mapcat (fn [[x & more]]
                 (map (partial vector x) more)))))

(defn flow-map-edges [g n]
  (->> (comb/combinations (u/nodes g) 2)
       (pmap #(apply lalg/max-flow g %))
       (some #(when (= n (second %)) (first %)))
       a/resolve-paths
       keys))

(defn p1 [in]
  (let [g (apply u/graph (parse-input in))]
    (->> (comb/combinations (flow-map-edges g 3) 3)
         (map #(u/remove-edges* g %))
         (pmap alg/connected-components)
         (some #(when (= 2 (count %))
                  (transduce (map (comp count distinct)) * %))))))
