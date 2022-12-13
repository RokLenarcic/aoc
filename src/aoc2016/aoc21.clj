(ns aoc2016.aoc21
  (:require [aoc.inputs :as inputs]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn swap-idx [v {:keys [p1 p2]}] (-> v (assoc p1 (v p2)) (assoc p2 (v p1))))
(defn swap [v {:keys [l1 l2]}] (mapv (fn [l] (get {l1 l2 l2 l1} l l)) v))
(defn rot [v {:keys [dir n]}]
  (let [s (count v)]
    (->> (cycle v)
         (drop (if (= dir "left") n (mod (- n) s)))
         (take s)
         (into []))))

(defn rot-letter [v {:keys [l]}]
  (let [idx (loop [i 0 v' v] (if (= l (first v')) i (recur (inc i) (next v'))))]
    (rot v {:dir "right" :n (+ 1 idx (if (< idx 4) 0 1))})))

(defn invert-rot-letter [v inst]
  (first
    (for [rot-idx (range (count v))
          :let [candidate (take (count v) (drop rot-idx (cycle v)))]
          :when (= v (rot-letter candidate inst))]
      (vec candidate))))

(defn reverse-p [v {:keys [p1 p2]}]
  (-> (subvec v 0 p1)
      (into (rseq (subvec v p1 (inc p2))))
      (into (subvec v (inc p2)))))

(defn move [v {:keys [from to]}]
  (if (< to from)
    (-> (subvec v 0 to)
        (conj (v from))
        (into (subvec v to from))
        (into (subvec v (inc from))))
    (-> (subvec v 0 from)
        (into (subvec v (inc from) (inc to)))
        (conj (v from))
        (into (subvec v (inc to))))))

(defn reverse-op [v {:keys [op] :as inst}]
  (case op
    :swap-idx (swap-idx v inst)
    :swap (swap v inst)
    :rot (rot v (update inst :dir {"left" "right" "right" "left"}))
    :rot-letter (invert-rot-letter v inst)
    :reverse (reverse-p v inst)
    :move (move v (set/rename-keys inst {:from :to :to :from}))))

(defn ops [v {:keys [op] :as inst}]
  (case op
    :swap-idx (swap-idx v inst)
    :swap (swap v inst)
    :rot (rot v inst)
    :rot-letter (rot-letter v inst)
    :reverse (reverse-p v inst)
    :move (move v inst)))

(defn parse-step [l]
  (inputs/reg-parse l
    [_ p1 p2] #"swap position (\d+) with position (\d+)" {:op :swap-idx :p1 (parse-long p1) :p2 (parse-long p2)}
    [_ p1 p2] #"swap letter (\w+) with letter (\w+)" {:op :swap :l1 (first p1) :l2 (first p2)}
    [_ dir n] #"rotate (left|right) (\d+) steps?" {:op :rot :dir dir :n (parse-long n)}
    [_ l] #"rotate based on position of letter (\w+)" {:op :rot-letter :l (first l)}
    [_ p1 p2] #"reverse positions (\d+) through (\d+)" {:op :reverse :p1 (parse-long p1) :p2 (parse-long p2)}
    [_ p1 p2] #"move position (\d+) to position (\d+)" {:op :move :from (parse-long p1) :to (parse-long p2)}))

(def input (->> (slurp "/Users/roklenarcic/aoc/aoc16/aoc21.txt")
                str/trim str/split-lines (map parse-step)))

(defn p1 [in] (apply str (reduce ops (vec "abcdefgh") in)))
(defn p2 [in code] (apply str (reduce reverse-op (vec code) (reverse in))))
