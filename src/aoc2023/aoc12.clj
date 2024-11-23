(ns aoc2023.aoc12
  (:require [aoc.colls :as c]
            [aoc.inputs :as in]
            [clojure.string :as str]))

(def test-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc12.txt"))

(defn parse-input [in] (->> in str/split-lines (mapv #(update (str/split % #"\s+") 1 (comp vec in/parse-numbers)))))
(defn multiply-input [in times]
  (mapv (fn [[pat cnt]] [(str/join \? (repeat times pat)) (vec (flatten (repeat times cnt)))]) in))

(defn group-match? "Match group of # or ?, followed by end of string or . or ?"
  [^String pattern i n]
  (-> #(let [c (.charAt pattern %2)]
         (if (< %1 n)
           (if (= \. c) (reduced -1) (inc %1))
           (reduced (when (not= \# c) %1))))
      (reduce 0 (range i (.length pattern)))
      (= n)))

(defn variants "Pat is pattern string as vec, counts is also vec."
  [^String pat counts]
  (let [last-hash (max (.lastIndexOf pat (int \#)) 0)
        f (c/memoize*
            (fn [f i j]
              (if-let [cnt (get counts j)]
                (if-let [c (when (< i (.length pat)) (.charAt pat i))]
                  (case c
                    \. (f f (inc i) j)
                    \# (if (group-match? pat i cnt) (f f (+ i cnt 1) (inc j)) 0)
                    \? (+ (if (group-match? pat i cnt) (f f (+ i cnt 1) (inc j)) 0)
                          (f f (inc i) j)))
                  (if (= j (count counts)) 1 0))
                (if (< last-hash i) 1 0))))]
    (f f 0 0)))

(defn p [in] (c/sum-of #(variants (first %) (second %)) in))
(defn p1 [in] (p (parse-input in)))
(defn p2 [in] (p (multiply-input (parse-input in) 5)))
