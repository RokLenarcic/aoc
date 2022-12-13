(ns aoc2021.aoc14
  (:require [clojure.string :as str]
            [aoc.colls :as c]))

(def test-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc14.txt")))

(defn start-pairs [s] (frequencies (mapv vector s (rest s))))
(defn count-chars [pairs start]
  (update-vals
    (reduce-kv
      (fn [acc [p1 p2] cnt]
        (-> acc
            (update p1 (fnil + 0) cnt)
            (update p2 (fnil + 0) cnt)))
      {(first start) 1 (last start) 1}
      pairs)
    #(/ % 2)))

(defn run-step [pairs productions]
  (->> pairs
       (mapv #(zipmap (productions (key %))
                      (repeat (val %))))
       (apply merge-with +)))

(defn parse-rules [in]
  (let [[start _ & productions] (str/split-lines in)]
    {:start start
     :productions (into {}
                        (map (fn [[p1 p2 mid]]
                               [[p1 p2] [[p1 mid] [mid p2]]]))
                        (mapv #(vec (str/replace % " -> " "")) productions))}))

(defn process [in]
  (let [{:keys [start productions]} (parse-rules in)]
    (->> (start-pairs start)
         (iterate #(run-step % productions))
         (map #(count-chars % start)))))

(defn p1 [in]
  (let [freqs (nth (process in) 10)]
    (- (reduce max (vals freqs))
       (reduce min (vals freqs)))))

(defn p2 [in]
  (let [freqs (nth (process in) 40)]
    (- (reduce max (vals freqs))
       (reduce min (vals freqs)))))
