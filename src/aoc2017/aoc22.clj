(ns aoc2017.aoc22
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as inputs]
    [clojure.string :as str]))

(def test-input "..#\n#..\n...")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc17/aoc22.txt")))

(defn parse-input [s] (inputs/map2d s #(if (= \# %) true false)))

(defn turn [{:keys [infections state p dir]}]
  (let [dir' (if (state p) (a/rot dir :R) (a/rot dir :L))]
    {:infections (cond-> infections (not (state p)) inc)
     :dir dir'
     :p (a/p-sum p dir')
     :state (update state p not)}))

(defn start-pos [state]
  (let [[_ [maxx maxy]] (a/bounds (keys state))]
    [(quot maxx 2) (quot maxy 2)]))

(defn p1 [in]
  (let [state (parse-input in)
        s {:infections 0
           :dir [-1 0]
           :p (start-pos state)
           :state state}]
    (:infections (nth (iterate turn s) 10000))))

(defn turn2 [{:keys [infections state p dir]}]
  (let [curr-state (state p :clean)
        dir' (case curr-state
               :infected (a/rot dir :R)
               :clean (a/rot dir :L)
               :weakened dir
               :flagged (-> dir (a/rot :L) (a/rot :L)))]
    {:infections (cond-> infections (= :weakened (state p)) inc)
     :dir dir'
     :p (a/p-sum p dir')
     :state (assoc state p ({:weakened :infected :infected :flagged :flagged :clean :clean :weakened} curr-state))}))

(defn p2 [in]
  (let [state (parse-input in)
        s {:infections 0
           :dir [-1 0]
           :p (start-pos state)
           :state (update-vals state #(if % :infected :clean))}]
    (:infections (nth (iterate turn2 s) 10000000))))
