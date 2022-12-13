(ns aoc2015.aoc15
  (:require [aoc.colls :as c]
            [aoc.math :as math]
            [clojure.string :as str]
            [medley.core :as m]))

(defn parse-ingredient [l]
  (let [[_ what capacity durability flavor texture calories] (re-find #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)" l)]
    (c/map-of what (parse-long capacity) (parse-long durability) (parse-long flavor) (parse-long texture) (parse-long calories))))
(def input
  (update-vals
    (->> (slurp "/Users/roklenarcic/aoc/aoc15/aoc15.txt")
         str/trim str/split-lines
         (map parse-ingredient)
         (m/index-by :what))
    #(dissoc % :what)))

(defn bake-cookie [cookie properties]
  (reduce-kv (fn [all-props k v]
               (merge-with + all-props (update-vals (properties k) #(* v %))))
             {}
             cookie))

(defn grade-cookie [baked-cookie]
  (if (every? pos-int? (vals baked-cookie))
    (reduce * (vals (dissoc baked-cookie :calories)))
    0))
(defn p1 [in]
  (->> (math/combinations-repeat (keys in) 100)
       (map #(bake-cookie % in))
       (map grade-cookie)
       (reduce max)))

(defn p2 [in]
  (->> (math/combinations-repeat (keys in) 100)
       (map #(bake-cookie % in))
       (filter #(= (:calories %) 500))
       (map grade-cookie)
       (reduce max)))
