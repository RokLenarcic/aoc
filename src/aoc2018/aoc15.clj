(ns aoc2018.aoc15
  (:require [aoc.inputs :as inputs]
            [aoc.arrays :as a]
            [aoc.chars :refer [char-square]]
            [clojure.string :as str]
            [medley.core :as m]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg])
  (:import (java.util  SortedSet TreeSet)))

(def test-input "###########\n#.G.#...G.#\n###..E#####\n###########")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc15.txt")))

(defn in->map [in]
  (inputs/map2d in #(when-not (= % \#) %)))

(def dir-cost {[-1 0] 0 [0 -1] 1 [0 1] 2 [1 0] 3})

(defn parse-map [in]
  (-> (in->map in) (update-vals (constantly nil)) (a/->MapKd (keys dir-cost)) a/array->graph))

(defn parse-people
  "Returns Sorted Map of pos -> person"
  [in]
  (reduce-kv
    (fn [acc k v]
      (case v
        \E (assoc acc k [true 200 3])
        \G (assoc acc k [false 200 3])
        acc))
    {}
    (in->map in)))

(defn apply-move [people p new-p]
  (-> people
      (assoc new-p (people p))
      (dissoc p)))

(defn shortest-path
  "Find shortest path, keeping path1 if equal"
  [path1 path2]
  (cond
    (nil? path1) path2
    (nil? path2) path1
    :else (if (< (quot (alg/cost-of-path path2) 1000) (quot (alg/cost-of-path path1) 1000))
            path2 path1)))

(defn next-move
  "People is a lookup map, field is a graph, and end-nodes should be a sorted set."
  [people field p end-nodes]
  (let [adjacent-nodes (into (sorted-set) (mapcat #(a/adjacent* a/dirs %) end-nodes))
        all-paths (alg/shortest-path
                    field
                    {:edge-filter (fn [edge] (let [x (u/dest edge)] (nil? (people x))))
                     :cost-fn (fn [edge]
                                (let [src (u/src edge)]
                                  (+ 1000 (if (= src p) (dir-cost (mapv - (u/dest edge) src)) 0))))
                     :start-node p
                     :heuristic-fn
                     (fn [n] (+ (* 1000 (abs (- (n 0) (p 0))))
                                (* 1000 (abs (- (n 1) (p 1))))))})]
    (some-> (reduce #(shortest-path %1 (alg/path-to all-paths %2)) nil adjacent-nodes)
            alg/edges-in-path
            first
            u/dest)))

(defn targets
  "Targets for person at p"
  [p people]
  (when-let [[elf? _] (people p)]
    (m/filter-vals #(not= elf? (first %)) people)))

(defn adjacent-attack
  "Attacks an adjacent target if possible, returns new-people and possibly erases target."
  [people p ^SortedSet turn-order]
  (let [[_ _ power] (people p)
        target (->> (a/adjacent* a/dirs p)
                    (select-keys (targets p people))
                    ;; sort by health and position
                    (sort-by #(vector (val %) (key %)))
                    ffirst)
        [_ hp] (people target)]
    (if hp
      (if (< power hp)
        (update-in people [target 1] - power)
        (do (.remove turn-order target)
            (dissoc people target)))
      people)))

(defn take-turn
  "Takes one turn with piece at p, returns new people map, reduced if no targets were
  available."
  [^SortedSet turn-order field people p]
  (let [targets (set (keys (targets p people)))]
    (if (seq targets)
      (let [m (next-move people field p targets)]
        (cond-> people
          m (apply-move p m)
          :always (adjacent-attack (or m p) turn-order)))
      (reduced (with-meta people {:done? true})))))

(defn take-round [people field]
  (let [turn-order (doto (TreeSet.) (.addAll (keys people)))]
    (reduce
      (partial take-turn turn-order field)
      people
      (take-while some? (repeatedly #(.pollFirst turn-order))))))

(defn print-state [people field]
  (a/print-2d-map
    (merge (zipmap (u/nodes field) (repeat \.))
           people)
    (fn [x]
      (if (= \. x)
        (char-square :brown)
        (if (true? (first x))
               (char-square :green)
               (if (false? (first x))
                 (char-square :red)
                 (char-square :black)))))))

(defn run-game [people field]
  (loop [rounds 0
         people people]
    (let [people (take-round people field)]
      (if (-> people meta :done?)
        [rounds people]
        (recur (inc rounds) people)))))

(defn game-result [res]
  (* (first res) (->> (second res) vals (mapv second) (reduce +))))

(defn flawless-elf-victory? [res initial-people]
  (= (count (m/filter-vals first initial-people))
     (count (m/filter-vals first (second res)))))

(defn p1 [in] (game-result (run-game
                             (parse-people in)
                             (parse-map in))))

(defn increase-elf-power [people]
  (update-vals people (fn [[elf? hp power]]
                        [elf? hp (if elf? (inc power) power)])))

(defn p2 [in]
  (let [people (parse-people in)
        field (parse-map in)]
    (->> (iterate increase-elf-power people)
         (map #(run-game % field))
         (filter #(flawless-elf-victory? % people))
         first
         game-result)))
