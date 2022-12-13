(ns aoc2016.aoc11
  (:require [aoc.colls :refer [memoize*]]
            [clojure.math.combinatorics :as comb])
  (:import (java.util HashSet LinkedList)))

(def test-input
  [{:chips #{:hydrogen :lithium}}
   {:gen #{:hydrogen}}
   {:gen #{:lithium}}
   {}])

(def input
  [{:gen #{:polonium :thulium :promethium :ruthenium :cobalt} :chips #{:thulium :ruthenium :cobalt}}
   {:gen #{} :chips #{:polonium :promethium}}
   {:gen #{} :chips #{}}
   {:gen #{} :chips #{}}])

(defn state-valid? [floor-state]
  (let [{:keys [gen chips]} floor-state]
    (or (empty? gen)
        (empty? chips)
        (empty? (apply disj chips gen)))))

(def state-valid?* (memoize* state-valid?))

(defn apply-items [floor-state flat-items f]
  (reduce
    (fn [s it]
      (update s (first it) f (second it)))
    floor-state
    flat-items))

(defn item-combos [{:keys [gen chips]}]
  (let [all (concat (map (fn [c] [:chips c]) chips) (map (fn [g] [:gen g]) gen))]
    (concat (comb/combinations all 2) (comb/combinations all 1))))

(defn items-to-move [floor-state]
  (for [items (item-combos floor-state)
        :let [new-s (apply-items floor-state items disj)]
        :when (state-valid?* new-s)]
    [items new-s]))

(def items-to-move* (memoize* items-to-move))

(defn moves-from [state floor-now]
  (for [floor-next [(inc floor-now) (dec floor-now)]
        :when (<= 0 floor-next 3)
        [items new-floor-now] (items-to-move* (state floor-now))
        :let [new-floor-next (apply-items (state floor-next) items (fnil conj #{}))]
        :when (state-valid?* new-floor-next)]
    [(-> state
         (assoc floor-next new-floor-next)
         (assoc floor-now new-floor-now))
     floor-next]))

(defn done? [state floor]
  (and (= floor 3) (every? #(and (empty? (:gen %)) (empty? (:chips %))) (butlast state))))

(defn enqueue [^LinkedList q {:keys [s f steps]}]
  (doseq [[s' f'] (moves-from s f)]
    (.offer q {:s s' :f f' :steps (inc steps)})))

(defn find-solution
  [state floor]
  (let [q (LinkedList.)
        seen (HashSet.)
        init {:s state :f floor :steps 0}]
    (.offer q init)
    (loop [{:keys [s f] :as it} (.poll q)]
      (cond
        (nil? it) (throw (Exception. "Not Found"))
        (not (.add seen (dissoc it :steps))) (recur (.poll q))
        (done? s f) (:steps it)
        :else (do (enqueue q it)
                  (recur (.poll q)))))))

(defn p1 [in] (find-solution in 0))
(defn p2 [in] (find-solution (-> in
                                 (update-in [0 :gen] conj :elerium :dilithium)
                                 (update-in [0 :chips] conj :elerium :dilithium))
                             0))
