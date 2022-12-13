(ns aoc2020.aoc21
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(defn general-allergy-possiblities
  "Returns a map of allergen to possible foods, which is the most general
  set of possibilities for each allergen, based on simple intersection of foods+allergens.

  This still doesn't deal with the fact that the same food cannot have more than 1 allergen."
  [in]
  (let [per-item (map (fn [{:keys [foods allergens]}]
                        (zipmap allergens (repeat foods)))
                      in)]
    (apply merge-with set/intersection per-item)))

(defn possibilities
  "Returns all possible mappings between allergen and food where a single allergen corresponds to a single food."
  [allergens general-map]
  (if (seq allergens)
    (let [a (first allergens)]
      (for [food (general-map a)
            p (possibilities (disj allergens a) general-map)]
        (assoc p a food food a)))
    [{}]))

(defn combine-possibles
  [valid-combos new-combos]
  (for [c1 valid-combos
        c2 new-combos
        :let [c' (reduce-kv
                   (fn [acc k v]
                     (if-let [v' (acc k)]
                       (if (= v v') acc (reduced nil))
                       (assoc acc k v)))
                   c1 c2)]
        :when c']
    c'))

(defn allergens-mappings [in]
  (let [general (general-allergy-possiblities in)
        combo-lists (map (fn [item] (possibilities (:allergens item) general)) in)]
    (reduce combine-possibles combo-lists)))

(defn parse-input [in]
  (->> in str/trim str/split-lines
       (map #(re-find #"(.*) \(contains (.*)\)" %))
       (map rest)
       (mapv (fn [[fds as]]
               (let [item {:foods (set (str/split fds #" "))
                           :allergens (set (str/split as #", "))}]
                 item)))))

(def test-input (parse-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc21.txt")))

(defn p1 [in]
  (let [might-have-allergens (into #{} (reduce concat (vals (general-allergy-possiblities in))))]
    (count (remove might-have-allergens (mapcat :foods in)))))

(defn p2 [in]
  (let [a (allergens-mappings in)
        all-allergens (dedupe (sort (mapcat :allergens in)))]
    (str/join \, (map (first a) all-allergens))))
