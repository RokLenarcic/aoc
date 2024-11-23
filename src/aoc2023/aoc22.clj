(ns aoc2023.aoc22
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc22.txt"))

(defn parse-brick [s]
  (let [[x y] (split-at 3 (mapv parse-long (str/split s #",|~")))]
    [(mapv min x y) (mapv max x y)]))

(defn bot-z [brick] (get-in brick [0 2]))

(defn parse-input [in] (->> in str/split-lines (mapv parse-brick) (sort-by bot-z) vec))

(defn intersects? "Only detects intersection of lower's upper edge and upper's lower edge"
  [[[lminx lminy _] [lmaxx lmaxy lz]] [[uminx uminy uz] [umaxx umaxy]]]
  (and (= lz uz)
       (<= (max uminx lminx) (min umaxx lmaxx))
       (<= (max uminy lminy) (min umaxy lmaxy))))

(defn move-down
  [brick]
  (-> brick
      (update 0 a/p-sum [0 0 -1])
      (update 1 a/p-sum [0 0 -1])))

(defn can-place? [bricks brick]
  (and (not= 0 (bot-z brick))
       (not-any? #(intersects? % brick) bricks)))

(defn settle-down [bricks]
  (reduce (fn [settled brick]
            (loop [brick brick]
              (let [newb (move-down brick)]
                (if (can-place? settled newb)
                  (recur newb)
                  (conj settled brick)))))
          []
          bricks))

(defn bricks-moved
  [bricks idx]
  (let [v (vec (m/remove-nth idx bricks))
        settled (settle-down v)]
    (c/count-matching true? (map not= v settled))))

(defn p [in]
  (let [in (parse-input in)
        settled (vec (sort-by bot-z (settle-down in)))]
    (pmap #(bricks-moved settled %) (range (count settled)))))

(defn p1 [in] (c/count-matching zero? (p in)))
(defn p2 [in] (c/sum-of (p in)))
