(ns aoc2017.aoc24
  (:require [clojure.string :as str]))

(def test-input "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10")
(def input (slurp "/Users/roklenarcic/aoc/aoc17/aoc24.txt"))

(defn parse-components [in]
  (->> in str/trim str/split-lines (map #(str/split % #"/")) (map #(mapv parse-long %)) set))

(def best-subchain
  (memoize
    (fn [outlet components]
      (reduce max
              (for [c components
                    :let [[c1 c2] c]]
                (cond (= outlet c1) (+ c1 c2 (best-subchain c2 (disj components c)))
                      (= outlet c2) (+ c1 c2 (best-subchain c1 (disj components c)))
                      :else 0))))))

(def best-longest-subchain
  (memoize
    (fn [outlet components]
      (let [extend-results (fn [[c1 c2] [len strength]]
                             [(inc len) (+ c1 c2 strength)])
            candidates (for [c components
                             :let [[c1 c2] c]]
                         (cond (= outlet c1) (extend-results c (best-longest-subchain c2 (disj components c)))
                               (= outlet c2) (extend-results c (best-longest-subchain c1 (disj components c)))
                               :else [0 0]))]
        (apply max-key (fn [[len strength]] (+ (* len 1000000) strength)) [0 0] candidates)))))

(defn p1 [in] (best-subchain 0 (parse-components in)))
(defn p2 [in] (best-longest-subchain 0 (parse-components in)))
