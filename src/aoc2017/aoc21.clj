(ns aoc2017.aoc21
  (:require [aoc.arrays :as a]
            [clojure.string :as str]))

(defn pat-coords [pat]
  (:acc
    (reduce
      (fn [{:keys [acc p]} c]
        (case c
          \/ {:acc acc :p (-> p (update 0 inc) (assoc 1 0))}
          \# {:acc (assoc acc p true) :p (update p 1 inc)}
          \. {:acc (assoc acc p false) :p (update p 1 inc)}))
      {:acc {} :p [0 0]}
      pat)))

(def input (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc17/aoc21.txt"))))
(def test-input ["../.# => ##./#../..."
                 ".#./..#/### => #..#/..../..../#..#"])

(defn all-variants
  [pat]
  (a/rotated-and-mirrored pat))

(defn add-pat [m s]
  (let [[_ from to] (re-find #"^(.*) => (.*)$" s)
        to-pat (pat-coords to)]
    (reduce
      (fn [acc pat]
        (assoc acc pat to-pat))
      m
      (all-variants (pat-coords from)))))

(defn parse-input [in] (reduce add-pat {} in))

;; above is the input patterns parsing
;; below is the running


(defn cut [state size]
  (reduce-kv
    (fn [acc p v]
      (assoc-in acc [(mapv #(quot % size) p) (mapv #(mod % size) p)] v))
    {}
    state))

(defn paste [superstate size]
  (reduce-kv
    (fn [acc quadrant points]
      (let [q-basis (mapv #(* % size) quadrant)]
        (merge acc (update-keys points #(a/p-sum q-basis %)))))
    {}
    superstate))

(defn expand [superstate expansions]
  (update-vals superstate #(or (expansions %)
                               (throw (ex-info "DONT KNOW " {:p %})))))

(defn step [state expansions]
  (let [[_ [maxx maxy]] (a/bounds (keys state))
        size (if (even? (inc maxx)) 2 3)]
    (-> state (cut size) (expand expansions) (paste (inc size)))))

(defn nth-state-lights [rules n]
  (let [expansions (parse-input rules)
        start (pat-coords ".#./..#/###")]
    (count (filter identity (vals (nth (iterate #(step % expansions) start) n))))))

(defn p1 [in] (nth-state-lights in 5))

(defn p2 [in] (nth-state-lights in 18))
