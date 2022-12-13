(ns aoc2016.aoc10
  (:require
    [aoc.inputs :as inputs]
    [aoc.colls :refer [memoize*]]
    [clojure.string :as str]
    [medley.core :as m]))

(def test-input "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2")
(def input (slurp "/Users/roklenarcic/aoc/aoc16/aoc10.txt"))

(defn parse-bots [in]
  (reduce
    (fn [bots l]
      (inputs/reg-parse l
        [_ v b] #"value (\d+) goes to bot (\d+)" (update-in bots [(keyword (str "bot" b)) :inputs] (fnil conj []) (parse-long v))
        [_ b t1 v1 t2 v2] #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)"
        (-> (update bots (keyword (str "bot" b)) merge {:low (keyword (str t1 v1)) :high (keyword (str t2 v2))}))))
    {}
    (str/split-lines (str/trim in))))

(defn invert-tree [bot-map]
  (let [low-edges (for [[n {:keys [low]}] bot-map] [low [:low n]])
        high-edges (for [[n {:keys [high]}] bot-map] [high [:high n]])
        input-edges (for [[n {:keys [inputs]}] bot-map in inputs] [n [:n in]])]
    (reduce
      (fn [acc [k v]] (update acc k (fnil conj []) v))
      {}
      (concat low-edges high-edges input-edges))))

(def grab-value
  (memoize*
    (fn [inv-tree i]
      (for [[t v] (inv-tree i)]
        (case t
          :low (reduce min (grab-value inv-tree v))
          :high (reduce max (grab-value inv-tree v))
          :n v)))))

(defn p1 [in v1 v2]
  (let [inv (invert-tree (parse-bots in))]
    (m/find-first
      #(let [[x1 x2] (grab-value inv %)]
         (or (and (= v1 x1) (= v2 x2))
             (and (= v1 x2) (= v2 x1))))
      (keys inv))))

(defn p2 [in]
  (let [inv (invert-tree (parse-bots in))]
    (apply * (flatten (map #(grab-value inv %) [:output0 :output1 :output2])))))
