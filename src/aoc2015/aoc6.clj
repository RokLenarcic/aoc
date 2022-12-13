(ns aoc2015.aoc6
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [clojure.string :as str]))

(defn square [x1 y1 x2 y2]
  (for [x (range (parse-long x1) (inc (parse-long x2)))
        y (range (parse-long y1) (inc (parse-long y2)))]
    [x y]))
(defn parse-line [l]
  (inputs/reg-parse l
    [_ x1 y1 x2 y2] #"turn on (\d+),(\d+) through (\d+),(\d+)" {:action :on :square (square x1 y1 x2 y2)}
    [_ x1 y1 x2 y2] #"turn off (\d+),(\d+) through (\d+),(\d+)" {:action :off :square (square x1 y1 x2 y2)}
    [_ x1 y1 x2 y2] #"toggle (\d+),(\d+) through (\d+),(\d+)" {:action :toggle :square (square x1 y1 x2 y2)}))

(defn parse-input [in] (map parse-line (str/split-lines in)))
(def input (parse-input (str/trim (slurp "/Users/roklenarcic/aoc/aoc15/aoc6.txt"))))

(defn p1 [in]
  (count
    (reduce
      (fn [on {:keys [action square]}]
        (case action
          :off (apply disj on square)
          :on (apply conj on square)
          :toggle (apply conj (apply disj on square) (remove on square))))
      #{}
      in)))

(defn update-bright [action acc p]
  (update acc p (fnil #(case action :off (max 0 (dec %)) :on (inc %) :toggle (+ 2 %)) 0)))

(defn p2 [in]
  (->> (reduce
         (fn [on {:keys [action square]}]
           (reduce (partial update-bright action) on square))
         {}
         in)
       (vals)
       (c/sum-of)))
