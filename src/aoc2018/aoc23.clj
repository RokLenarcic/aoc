(ns aoc2018.aoc23
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [aoc.inputs :as inputs]
            [aoc.math :as math]
            [clojure.string :as str])
  (:import (java.util Comparator PriorityQueue)))

(def test-input "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc23.txt")))

(defn parse-input [in]
  (->> (map (partial inputs/parse-numbers true) (str/split-lines in))
       (mapv (fn [[x y z r]] {:p [x y z] :r r}))))

(defn distance [p1 p2] (->> (mapv - p1 p2) (mapv abs) (reduce +)))

(defn p1 [in]
  (let [bots (parse-input in)
        best-bot (apply max-key :r bots)]
    (c/count-matching #(<= (distance (:p best-bot) (:p %)) (:r best-bot)) bots)))


(defn bot-box [{:keys [p r]}]
  [(a/p-sum p [(- r) (- r) (- r)])
   (a/p-sum p [r r r])])

(defn closest-point-3d
  "Closest point in a box to p"
  [[[minx miny minz] [maxx maxy maxz]] p]
  [(min (max (p 0) minx) maxx)
   (min (max (p 1) miny) maxy)
   (min (max (p 2) minz) maxz)])

(defn intersects?
  "Box intersects with a bot at any point"
  [box {:keys [p r]}]
  (<= (distance p (math/closest-point box p)) r))

(defn ranked-box
  "Rank a box according to "
  [box bots]
  (let [bots-intersecting (c/count-matching #(intersects? box %) bots)
        single? (every? zero? (mapv - (second box) (first box)))
        dist-to-origin (if single? (distance [0 0 0] (first box)) 0)]
    {:box box :single? single? :rank [(- bots-intersecting) dist-to-origin]}))

(defn split-box
  "Split box along longest axis"
  [[mins maxes]]
  (let [lengths (mapv - maxes mins)
        longest (reduce max lengths)]
    (loop [i 0]
      (if (= (lengths i) longest)
        (let [half (+ (mins i) (quot (lengths i) 2))]
          [[mins (assoc maxes i half)]
           [(assoc mins i (inc half)) maxes]])
        (recur (inc i))))))

(defn solve [bots]
  (let [^Comparator c #(compare (:rank %1) (:rank %2))
        q (PriorityQueue. c)]
    (loop [{:keys [box single?]} (ranked-box (a/bounds (mapcat bot-box bots)) bots)]
      (if single?
        (first box)
        (do (doseq [box (split-box box)]
              (.offer q (ranked-box box bots)))
            (recur (.poll q)))))))

(defn p2 [in] (distance [0 0 0] (solve (parse-input in))))
