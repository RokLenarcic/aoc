(ns aoc2018.aoc24
  (:require [aoc.colls :as c]
            [aoc.inputs :as inputs]
            [clojure.string :as str]
            [medley.core :as m])
  (:import (java.util Comparator PriorityQueue)))

(def test-input "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc24.txt")))

(defn parse-specials [s]
  (when s
    (let [weakness (some-> (re-find #"weak to ([^;)]+)" s) second (str/split #", ") set)
          immunity (some-> (re-find #"immune to ([^;)]+)" s) second (str/split #", ") set)]
      {:weak weakness
       :immune immunity})))

(defn ->units [{:keys [power dmg]}] (quot @power dmg))

(defn parse-line [team boost l]
  (when-let [[_ units hp specials dmg dmg-type init] (re-find #"(\d+) units each with (\d+) hit points (\([^)]+\))? ?with an attack that does (\d+) (\w+) damage at initiative (\d+)" l)]
    (let [{:keys [weak immune]} (parse-specials specials)
          dmg (+ boost (parse-long dmg))]
      (-> (c/map-of (parse-long units) (parse-long hp) (or weak #{}) (or immune #{}) dmg dmg-type (parse-long init) team)
          (assoc :power (volatile! (* dmg (parse-long units))))))))

(defn parse-input [in boost]
  (let [[[_ & immune-groups] [_ & infection-groups]] (inputs/blocks in)]
    (concat (mapv (partial parse-line :immune boost) immune-groups)
            (mapv (partial parse-line :infection 0) infection-groups))))

(defn targeting-order
  "Defines order in which groups choose their target."
  [x y]
  (- (compare [@(:power x) (:init x)] [@(:power y) (:init y)])))

(defn damage-order
  [damage-type]
  (fn [x y]
    (- (compare [(some? ((:weak x) damage-type)) @(:power x) (:init x)]
                [(some? ((:weak y) damage-type)) @(:power y) (:init y)]))))

(def damage-types ["cold" "fire" "radiation" "slashing" "bludgeoning"])
(def other-team {:infection :immune :immune :infection})

(defn add-to-team-map [by-team {:keys [immune team] :as g}]
  (doseq [dmg-type damage-types
          :when (not (immune dmg-type))]
    (update-in by-team [team dmg-type] #(.add ^PriorityQueue %1 g))))

(defn target-select [groups]
  (let [target-order (sort-by identity targeting-order groups)
        by-team {:immune (zipmap damage-types
                                 (map #(PriorityQueue. ^Comparator (damage-order %)) damage-types))
                 :infection (zipmap damage-types
                                    (map #(PriorityQueue. ^Comparator (damage-order %)) damage-types))}]
    (doseq [g groups] (add-to-team-map by-team g))
    (reduce (fn [ret {:keys [team dmg-type] :as attacker}]
              (let [defenders (by-team (other-team team))]
                (if-let [defender (.poll ^PriorityQueue (defenders dmg-type))]
                  (do (doseq [pq (vals defenders)]
                        (.remove ^PriorityQueue pq defender))
                      (assoc ret attacker defender))
                  (assoc ret attacker nil))))
            {}
            target-order)))

;;;;

(defn inflict-damage [{:keys [power dmg-type]} {:keys [immune weak hp] :as defender}]
  (if hp
    (let [power (cond-> @power
                  (weak dmg-type) (* 2)
                  (immune dmg-type) (* 0))
          units-killed (quot power hp)
          def-power (:power defender)]
      (vreset! def-power (max 0 (- @def-power (* (:dmg defender) units-killed))))
      units-killed)
    0))

(defn attack-phase [targets]
  (reduce (fn [acc attacker] (+ acc (inflict-damage attacker (targets attacker))))
          0
          (sort-by (comp - :init) (keys targets))))

(defn round [groups]
  (let [units-killed (attack-phase (target-select groups))]
    (when (pos-int? units-killed)
      (filterv #(pos-int? @(:power %)) groups))))

(defn winner [groups]
  (if groups
    (let [x (:team (first groups))]
      (when (every? #(= x (:team %)) groups) x))
    :tie))

(defn winning-units [in boost]
  (->> (iterate round (parse-input in boost))
       (m/find-first winner)
       (map ->units)
       (reduce +)))

(defn p1 [in] (winning-units in 0))

(defn find-lowest-boost [in min-boost max-boost]
  (if (= min-boost max-boost)
    min-boost
    (let [n (quot (+ min-boost max-boost) 2)]
      (if (= :immune (some winner (iterate round (parse-input in n))))
        (recur in min-boost n)
        (recur in (inc n) max-boost)))))

(defn p2 [in] (winning-units in (find-lowest-boost in 0 2000000)))
