(ns aoc2018.aoc17
  (:require [aoc.arrays :as a]
            [aoc.chars :refer [shapes]]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc17.txt")))

(defn parse-line [l]
  (let [[_ axis axis-pos _ range-start range-end] (re-find #"(\w)=(\d+), (\w)=(\d+)\.\.(\d+)" l)
        point (repeat (parse-long axis-pos))
        line (range (parse-long range-start) (inc (parse-long range-end)))]
    (zipmap
      (case axis
        "x" (mapv vector line point)
        "y" (mapv vector point line))
      (repeat \#))))

(defn blocked? [field p] (#{\# \~} (field p)))
(def render (comp shapes {nil [:square :black] \~ [:square :blue] \# [:square :brown] \| [:circle :blue]}))

(defn x-limits
  "Looks at span on x axis left and right from point p.

  Returns
  - enclosed? true/false if both sides are closed by walls
  - left, index of left wall or chasm
  - right, index of right wall of chasm"
  [field p]
  (let [y (p 0) x (p 1)
        limit (fn [indices]
                (some #(cond
                         (blocked? field [y %]) [true %]
                         (not (blocked? field [(inc y) %])) [false %]) indices))
        [block-left? idx-left] (limit (range (dec x) Long/MIN_VALUE -1))
        [block-right? idx-right] (limit (range (inc x) Long/MAX_VALUE))]
    (if (and block-left? block-right?)
      [true idx-left idx-right]
      [false idx-left idx-right])))

(defn drop-droplet [field p max-y]
  (let [np (update p 0 inc)]
    (if (> (np 0) max-y)
      field
      (if (blocked? field np)
        (let [[enclosed? left right] (x-limits field p)
              new-f (into field (map (fn [x] [(assoc p 1 x) (if enclosed? \~ \|)])) (range (inc left) right))
              left (assoc p 1 left)
              right (assoc p 1 right)]
          (if enclosed?
            (recur new-f (update p 0 dec) max-y)
            (cond-> new-f
              (not (blocked? new-f left)) (-> (assoc left \|) (drop-droplet left max-y))
              (not (blocked? new-f right)) (-> (assoc right \|) (drop-droplet right max-y)))))
        (if (= \| (field np))
          field
          (recur (assoc field np \|) np max-y))))))

(defn parse-input [in] (reduce merge (map parse-line (str/split-lines in))))

(defn simulate [field]
  (let [[[min-y _] [max-y _]] (a/bounds (keys field))
        ret (drop-droplet field [0 500] max-y)]
    (a/print-2d-map ret render)
    (m/filter-keys #(>= (first %) min-y) ret)))

(defn p1 [in]
  (->> (parse-input in) simulate (m/filter-vals #(#{\~ \|} %)) count))

(defn p2 [in]
  (->> (parse-input in) simulate (m/filter-vals #(= \~ %)) count))
