(ns aoc2018.aoc8
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def test-input (inputs/parse-numbers "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
(def input (inputs/parse-numbers (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc8.txt"))))

(defn reader [s]
  (let [v (vec s)
        a (atom -1)]
    (fn [] (v (swap! a inc)))))

(defn parse [read-int]
  (let [n-child (read-int)
        n-metadata (read-int)
        children (vec (repeatedly n-child #(parse read-int)))
        metadata (vec (repeatedly n-metadata #(read-int)))]
    {:children children
     :meta metadata}))

(defn parse-input [in]
  (parse (reader in)))

(defn sum-walk [{:keys [meta children] :as n}]
  (if children (reduce + (concat meta children)) n))

(defn p2-walk [{:keys [meta children] :as n}]
  (cond
    (seq children) (->> meta
                        (keep #(get children (dec %)))
                        (reduce +))
    meta (reduce + meta)
    :else n))

(defn p1 [in] (walk/postwalk sum-walk (parse-input in)))
(defn p2 [in] (walk/postwalk p2-walk (parse-input in)))
