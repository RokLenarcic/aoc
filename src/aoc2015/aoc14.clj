(ns aoc2015.aoc14
  (:require [aoc.colls :as c]
            [clojure.string :as str]))
(defn parse-reindeer [l]
  (let [[_ who speed duration sleep] (re-find #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." l)]
    (c/map-of who (parse-long speed) (parse-long duration) (parse-long sleep))))

(def input (map parse-reindeer (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc15/aoc14.txt")))))
(def test-input [{:who "Comet", :speed 14, :duration 10, :sleep 127}
                 {:who "Dancer", :speed 16, :duration 11, :sleep 162}])
(defn distance [{:keys [speed duration sleep]} t]
  (loop [t* t ret 0]
    (if (pos-int? t*)
      (recur (- t* duration sleep)
             (long (+ ret (* speed (min duration t*)))))
      ret)))

(defn p1 [in] (reduce max (map #(distance % 2503) in)))
(defn p2 [in]
  (frequencies
    (for [t (range 1 (inc (inc 2503)))
          :let [results (group-by #(distance % t) in)
                m (reduce max (keys results))]
          lead (results m)]
      (:who lead))))
