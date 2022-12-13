(ns aoc2015.aoc13
  (:require
    [aoc.colls :as c]
    [cljc-shuffle.core :as shuffle]
    [clojure.string :as str]))

(defn parse-input [in]
  (reduce
    (fn [acc l]
      (let [[_ who what how-much whom] (re-find #"^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)." l)]
        (assoc-in acc [who whom] (cond-> (parse-long how-much) (= what "lose") (* -1)))))
    {}
    (str/split-lines (str/trim in))))
(def test-input (parse-input "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol."))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc15/aoc13.txt")))

(defn grade [in v]
  (+ (get-in in [(last v) (first v)] 0)
     (get-in in [(first v) (last v)] 0)
     (c/sum-of
       (map (fn [p1 p2]
              (+ (get-in in [p1 p2] 0) (get-in in [p2 p1] 0))) v (rest v)))))

(defn best-seating [in people]
  (reduce max (map #(grade in (shuffle/apply-shuffle % people)) (range (shuffle/total-shuffles people)))))

(defn p1 [in] (best-seating in (keys in)))
(defn p2 [in] (best-seating in (cons "Me" (keys in))))
