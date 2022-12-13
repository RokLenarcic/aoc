(ns aoc2021.aoc23
  (:require [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(def test-input
  ;; docks are inverted with first item describing type
  [nil nil [\A \A \B] nil [\B \D \C] nil [\C \C \B] nil [\D \A \D] nil nil])

(def input
  [nil nil [\A \D \B] nil [\B \A \B] nil [\C \A \C] nil [\D \C \D] nil nil])

(def test-input-expanded
  ;; docks are inverted with first item describing type
  [nil nil [\A \A \D \D \B] nil [\B \D \B \C \C] nil [\C \C \A \B \B] nil [\D \A \C \A \D] nil nil])

(def input-expanded
  [nil nil [\A \D \D \D \B] nil [\B \A \B \C \B] nil [\C \A \A \B \C] nil [\D \C \C \A \D] nil nil])

(defn grab-val
  "Takes value from state at index if there's a value there to be taken. So not empty and
  not an element in correct dock. Returns [new-state val]"
  [state idx]
  (let [v (state idx)]
    (if (instance? Character v)
      [(assoc state idx nil) v]
      (when (apply not= (first v) (rest v))
        [(update state idx pop) (last v)]))))

(defn deposit-val
  "Deposit value into state at idx, returning new state or nil if invalid index."
  [state idx v]
  (let [curr-val (state idx)]
    (if (and (vector? curr-val) (apply = v curr-val))
      (assoc state idx (conj curr-val v))
      (when (nil? curr-val) (assoc state idx v)))))

(defn valid-path?
  "Returns true if path is valid, so no items between indexes and at most one point is non-dock."
  [state start-idx end-idx]
  (and (or (vector? (state start-idx)) (vector? (state end-idx)))
       (not-any? #(instance? Character (state %))
                 (range (inc (min start-idx end-idx)) (max start-idx end-idx)))))

(def weight-mul {\A 1 \B 10 \C 100 \D 1000})
(defn moves
  "Start and end index point to nil or vector. That is why intermediate state is used where the
  crab is gone from initial position."
  [intermediate-state start-idx end-idx dock-size]
  (let [dock-moves #(if (vector? %) (- dock-size (count %)) 0)]
    (+ (abs (- start-idx end-idx))
       (dock-moves (intermediate-state start-idx))
       (dock-moves (intermediate-state end-idx)))))

(defn apply-move [dock-size state [start-idx end-idx]]
  (when-let [[intermediate-state v]
             (and (valid-path? state start-idx end-idx)
                  (grab-val state start-idx))]
    (when-let [end-state (deposit-val intermediate-state end-idx v)]
      {:dest end-state
       :weight (* (weight-mul v) (moves intermediate-state start-idx end-idx dock-size))})))

(def indexes (vec (for [i (range 0 11) j (range 0 11) :when (not= i j)] [i j])))

(defn find-path [initial-state]
  (let [dock-size (reduce max (map count initial-state))
        end-state (-> (vec (repeat 11 nil))
                      (assoc 2 (vec (repeat dock-size \A)))
                      (assoc 4 (vec (repeat dock-size \B)))
                      (assoc 6 (vec (repeat dock-size \C)))
                      (assoc 8 (vec (repeat dock-size \D))))]
    (alg/cost-of-path
      (alg/shortest-path (fn [state] (keep (partial apply-move dock-size state) indexes))
                         initial-state
                         end-state
                         :weight))))
