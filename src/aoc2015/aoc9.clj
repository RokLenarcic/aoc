(ns aoc2015.aoc9
  (:require [clojure.string :as str]))
(def test-input "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141")
(def input (slurp "/Users/roklenarcic/aoc/aoc15/aoc9.txt"))

(defn distances [in]
  (reduce
    (fn [dists l]
      (let [[_ from to dist] (re-find #"(\w+) to (\w+) = (\d+)" l)]
        (-> dists (assoc-in [from to] (parse-long dist)) (assoc-in [to from] (parse-long dist)))))
    {}
    (str/split-lines (str/trim in))))

(defn find-route
  "Looks at distances, a map of distances from current point to every next point, and all distances at all"
  ([acc-fn distances]
   (let [locs (set (keys distances))]
     (find-route acc-fn nil locs distances)))
   ([acc-fn current-loc rem-locs all-distances]
    (if (seq rem-locs)
      (let [next-dists (all-distances current-loc (constantly 0))]
        (apply acc-fn
               (for [loc rem-locs]
                 (+ (next-dists loc) (find-route acc-fn loc (disj rem-locs loc) all-distances)))))
      0)))

(defn p1 [in] (find-route min (distances in)))
(defn p2 [in] (find-route max (distances in)))
