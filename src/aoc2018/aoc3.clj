(ns aoc2018.aoc3
  (:require [aoc.inputs :as inputs]
            [medley.core :as m]
            [clojure.string :as str]))

(def test-input "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")
(def input (slurp "/Users/roklenarcic/aoc/aoc18/aoc3.txt"))

(defn parse-claim [l]
  (inputs/reg-parse l
    [_ id x y xlen ylen]
    #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    {:id (parse-long id)
     :square [[(parse-long x) (parse-long y)]
              [(+ (parse-long x) (parse-long xlen)) (+ (parse-long y) (parse-long ylen))]]}))


(defn parse-input [in] (map parse-claim (str/split-lines in)))

(defn coords [claim]
  (for [x (range (-> claim :square ffirst)
                 (-> claim :square second first))
        y (range (-> claim :square first second)
                 (-> claim :square second second))]
    [x y]))

(defn overlaps [claims]
  (apply merge-with + (map #(zipmap (coords %) (repeat 1)) claims)))

(defn p1 [in]
  (count (m/filter-vals #(< 1 %) (overlaps (parse-input in)))))

(defn p2 [in]
  (let [claims (parse-input in)
        overlaps (overlaps claims)]
    (for [claim claims
          :when (every? #(= 1 (overlaps %)) (coords claim))]
      claim)))
