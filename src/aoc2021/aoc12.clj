(ns aoc2021.aoc12
  (:require [aoc.inputs :as inputs]
            [aoc.colls :as c]
            [clojure.string :as str]))

(def test-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc12.txt")))

(defn cave-type [cave]
  (cond
    (= cave "start") :special
    (= cave "end") :special
    (Character/isUpperCase ^Character (first cave)) :big
    :else :small))

(defn can-visit? [double-visit? visits p]
  (let [double-open? (not-any? #(and (= (cave-type (key %)) :small) (= 2 (val %))) visits)
        p-visits (visits p 0)]
    (case (cave-type p)
      :special (zero? p-visits)
      :big true
      :small (or (zero? p-visits)
                 (and double-visit? double-open? (= 1 p-visits))))))

(defn dfs-special
  "Dfs with limit to visits special rules"
  [visits edges point double-visit?]
  (let [visits (update visits point (fnil inc 0))]
    (if (= "end" point)
      1
      (c/sum-of #(dfs-special visits edges % double-visit?)
                (filter #(can-visit? double-visit? visits %) (edges point))))))

(defn parse-input [in]
  (reduce
    (fn [acc [p1 p2]]
      (-> acc
          (update p1 (fnil conj []) p2)
          (update p2 (fnil conj []) p1)))
    {}
    (first (inputs/blocks in :item-fn #(str/split % #"-")))))

(defn p1 [in]
  (let [edges (parse-input in)]
    (dfs-special {} edges "start" false)))

(defn p2 [in]
  (let [edges (parse-input in)]
    (dfs-special {} edges "start" true)))
