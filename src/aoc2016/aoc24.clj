(ns aoc2016.aoc24
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as inputs]
    [ubergraph.core :as u]
    [ubergraph.alg :as alg]))

(def test-input "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########")
(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc24.txt"))

(defn parse-maze [s] (inputs/map2d s #(when (not= \# %) %)))

(defn shortest-route [curr dist unvisited end-cost-fn]
  (if (empty? unvisited)
    (end-cost-fn curr)
    (let [dists (dist curr)]
      (reduce
        (fn [ret p]
          (min ret (+ (dists p) (shortest-route p dist (disj unvisited p) end-cost-fn))))
        Long/MAX_VALUE
        unvisited))))

(defn distances [in]
  (let [g (-> in parse-maze (a/->MapKd a/dirs) a/array->graph)
        points (into {} (for [n (u/nodes g)
                              :when (not= \. (u/attr g n :v))]
                          [(u/attr g n :v) n]))]
    (update-vals
      points
      (fn [p]
        (let [others (alg/shortest-path g {:start-node p})]
          (reduce-kv
            #(if (= %3 p)
               %1
               (assoc %1 %2 (alg/cost-of-path (alg/path-to others %3))))
            {}
            points))))))

(defn p1 [in]
  (let [dist (distances in)]
    (shortest-route \0 dist (disj (into #{} (keys dist)) \0) (constantly 0))))

(defn p2 [in]
  (let [dist (distances in)]
    (shortest-route \0 dist (disj (into #{} (keys dist)) \0) #(get-in dist [% \0]))))
