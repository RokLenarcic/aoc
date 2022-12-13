(ns aoc2015.aoc4
  (:require
    [aoc.hash :refer [md5-stream]]
    [clojure.string :as str]
    [medley.core :as m]))

(def input "ckczppom")

(defn hash-with-leading-zeros [in zeros]
  (let [prefix (apply str (repeat zeros \0))]
    (->> (m/indexed (md5-stream in 1))
         (m/find-first #(str/starts-with? (second %) prefix))
         first)))

(defn p1 [in] (hash-with-leading-zeros in 5))
(defn p2 [in] (hash-with-leading-zeros in 6))
