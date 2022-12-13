(ns aoc2016.aoc5
  (:require
    [aoc.hash :refer [md5-stream]]
    [clojure.string :as str]))

(def input "wtnhxymk")

(defn p1 [in]
  (->> (md5-stream in 1)
       (filter #(str/starts-with? % "00000"))
       (map #(nth % 5))
       (take 8)
       (apply str)))

(defn p2 [in]
  (reduce (fn [acc h]
            (if (not-any? nil? acc)
              (reduced acc)
              (let [pos (Long/parseLong (str (nth h 5)) 16)]
                (if (<= 0 pos 7)
                  (update acc pos #(or %1 %2) (nth h 6))
                  acc))))
          [nil nil nil nil nil nil nil nil]
          (->> (md5-stream in 1)
               (filter #(str/starts-with? % "00000")))))
