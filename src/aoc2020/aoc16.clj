(ns aoc2020.aoc16
  (:require
    [aoc.arrays :as a]
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [clojure.string :as str]
    [memento.config :as memc]
    [memento.core :as mem]))

(defn fields [lines]
  (reduce
    (fn [m l]
      (let [[_ field from1 to1 from2 to2] (re-find #"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)"l)]
        (assoc m field [[(parse-long from1) (parse-long to1)]
                        [(parse-long from2) (parse-long to2)]])))
    {}
    lines))

(defn matches? [intervals n] (some #(<= (first %) n (second %)) intervals))
(defn matches-some-field? [fields n] (some #(matches? % n) (vals fields)))

(defn parse-input [in]
  (inputs/blocks
    in
    :block-fn
    (fn [_ lines]
      (case (first lines)
        "your ticket:" (inputs/parse-numbers (second lines))
        "nearby tickets:" (map inputs/parse-numbers (rest lines))
        (fields lines)))))

(def test-input (parse-input "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"))
(def test-input2 (parse-input "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc16.txt")))

(defn p1 [[fields _ other-tickets]]
  (c/sum-of
    (for [ticket other-tickets
          v ticket
          :when (not-any? #(matches? % v) (vals fields))]
      v)))

(defn valid-tickets [fields other-tickets]
  (filter (partial every? #(matches-some-field? fields %)) other-tickets))

(defn fit-fields [fields columns]
  (if (seq columns)
    (reduce-kv
      (fn [_ field-name intervals]
        (when-let [other (and (every? #(matches? intervals %) (first columns))
                              (fit-fields (dissoc fields field-name) (rest columns)))]
          (reduced (cons field-name other))))
      nil
      fields)
    []))

(mem/memo #'fit-fields {memc/type memc/caffeine})

(defn p2 [[fields my-ticket other-tickets]]
  (let [valid (map vec (valid-tickets fields other-tickets))
        columns (a/rows-to-cols valid)]
    (reduce-kv
      (fn [acc n v]
        (if (str/includes? n "departure") (* acc v) acc))
      1
      (zipmap (fit-fields fields columns) my-ticket))))
