(ns aoc2018.aoc13
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs])
  (:import (java.util Map TreeMap)))

(def test-input "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   ")
(def input (slurp "/Users/roklenarcic/aoc/aoc18/aoc13.txt"))

(defn parse-positions [in] (inputs/map2d in {\> [0 1] \< [0 -1] \^ [-1 0] \v [1 0]}))
(defn parse-track [in] (inputs/map2d in #(get {\< \- \> \- \v \| \^ \| \space nil} % %)))

(defn track [field pos {:keys [dir next-branch]}]
  (let [next-pos (mapv + pos dir)]
    {:pos next-pos
     :dir (case (field next-pos)
            \/ (case dir [0 1] [-1 0] [-1 0] [0 1] [1 0] [0 -1] [0 -1] [1 0])
            \\ (case dir [0 1] [1 0] [1 0] [0 1] [-1 0] [0 -1] [0 -1] [-1 0])
            \+ (case next-branch
                 :l (a/rot dir :L)
                 :r (a/rot dir :R)
                 dir)
            dir)
     :next-branch (if (= \+ (field next-pos)) ({:l :s :s :r :r :l} next-branch) next-branch)}))

(defn parse-input [in]
  (inputs/map2d in #(when-not (= % \space) %)))

(def crashes (atom []))

(defn tick [field ^Map cart-map]
  (let [ret (doto (TreeMap.) (.putAll cart-map))]
    (doseq [[pos more] (.entrySet cart-map)
            :when (.get ret pos)]
      (.remove ret pos)
      (let [{:keys [pos dir next-branch]} (track field pos more)]
        (if (.get ret pos)
          (do
            (swap! crashes conj pos)
            (.remove ret pos))
          (.put ret pos {:next-branch next-branch :dir dir}))))
    ret))

(defn run-tracks [in]
  (let [track (parse-track in)
        carts (update-vals (parse-positions in) (fn [dir] {:dir dir :next-branch :l}))]
    (loop [carts (reduce-kv (fn [^Map m k v] (doto m (.put k v))) (TreeMap.) carts)]
      (if (<= (count carts) 1)
        carts
        (recur (tick track carts))))))

(defn p1 [in]
  (reset! crashes [])
  (run-tracks in)
  (reverse (first @crashes)))

(defn p2 [in]
  (reset! crashes [])
  (reverse (key (first (run-tracks in)))))
