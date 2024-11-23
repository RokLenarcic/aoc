(ns aoc2023.aoc20
  (:require [aoc.colls :as c]
            [aoc.math :as math]
            [clojure.string :as str]
            [medley.core :as m])
  (:import (clojure.lang PersistentQueue)))

(def test-input "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc20.txt"))

(defn parse-module [m]
  (let [[_ op id dests] (re-find #"([%&])?(\w+) -> (.*)" m)
        process-dests #(mapv keyword (str/split % #",\s*"))]
    (cond-> (c/map-of op (keyword id) (process-dests dests))
      (= op "%") (assoc :state false))))

(defn parse-input [in]
  (let [modules (mapv parse-module (str/split-lines in))]
    (reduce #(assoc-in %1 %2 false)
            (m/index-by :id modules)
            (for [{:keys [dests id]} modules
                  dest dests]
              [dest :src id]))))

(defn engage-module [{:keys [op dests id] :as module} src-module level]
  (let [output-signals #(mapv (fn [d] [id % d]) dests)]
    (case op
      "%" (if level
            [module []]
            (let [new-state (not (:state module))]
              [(assoc module :state new-state) (output-signals new-state)]))
      "&" (let [sources (assoc (:src module) src-module level)]
            (if (every? true? (vals sources))
              [(assoc module :src sources) (output-signals false)]
              [(assoc module :src sources) (output-signals true)]))
      nil [module (output-signals level)])))

(defn run-modules [modules]
  (loop [q (conj PersistentQueue/EMPTY [nil false :broadcaster])
         modules modules
         sent {false 0 true 0}]
    (if-let [[src level dest] (peek q)]
      (let [[next-module signals] (engage-module (modules dest) src level)]
        (recur (apply conj (pop q) signals) (assoc modules dest next-module) (update sent level inc)))
      (with-meta modules {:sent sent}))))

(defn p1 [in]
  (->> (parse-input in)
       (iterate run-modules)
       (take 1001)
       (mapv (comp :sent meta))
       (reduce (partial merge-with +))
       vals
       (apply *)))

(defn branch? [modules {:keys [op src]}] (and (some #(= "%" (get-in modules [% :op])) (keys src))
                                              (= op "&")
                                              (> (count src) 1)))

(defn extract-state [modules nodes] (update-vals (select-keys modules nodes) :src))

(defn p2 [in]
  (let [modules (parse-input in)
        important-nodes (keys (m/filter-vals (partial branch? modules) modules))
        states (map #(extract-state % important-nodes) (iterate run-modules modules))]
    (->> important-nodes
         (mapv #(c/find-cycle-of-seq (map % states) 10 10000))
         (mapv :length)
         (reduce math/lcm))))
