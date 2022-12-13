(ns aoc2016.aoc14
  (:require
    [aoc.hash :refer [md5-stream]]
    [clojure.string :as str]
    [medley.core :as m]))

(def input "ngcjuoqr")

(defn cipher-stream [s]
  (loop [[h & more] s]
    (if-let [[_ c] (re-find #"(.)\1\1" (second h))]
      (let [five (apply str (repeat 5 c))]
        (if (some #(str/includes? % five) (take 1000 (map second more)))
          (cons h (lazy-seq (cipher-stream more)))
          (recur more)))
      (recur more))))

(defn p1 [in] (nth (cipher-stream (m/indexed (md5-stream in 1))) 63))
(defn p2 [in] (nth (cipher-stream (m/indexed (md5-stream in 2017))) 63))
