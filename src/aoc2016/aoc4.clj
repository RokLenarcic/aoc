(ns aoc2016.aoc4
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(defn real? [{:keys [enc chksm]}]
  (->> (frequencies (remove #{\-} enc))
       (sort-by (juxt (comp unchecked-negate second) first))
       (take 5)
       (map first)
       (apply str)
       (= chksm)))

(defn parse-room [r]
  (let [[_ enc sector chksm] (re-find #"^([a-z-]+)(\d+)\[([a-z]+)\]$" r)]
    (c/map-of enc (parse-long sector) chksm)))

(def input (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc16/aoc4.txt"))))

(defn rot [n c]
  (-> (- (int c) 97)
      (+ n)
      (mod 26)
      (+ 97)
      char))

(defn decrypt [{:keys [enc sector]}]
  (str (apply str (map #(if (= \- %) % (rot sector %)) enc)) sector))

(defn p1 [in]
  (->> (map parse-room in)
       (filter real?)
       (map :sector)
       (reduce +)))

(defn p2 [in]
  (->> (map parse-room in)
       (map decrypt)))
