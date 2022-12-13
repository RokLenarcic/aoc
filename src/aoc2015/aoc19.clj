(ns aoc2015.aoc19
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str])
  (:import (java.util HashMap)))

(defn parse [s]
  (let [[rules molecule] (inputs/blocks s
                                        :item-fn
                                        #(if-let [[_ from to] (re-find #"(\w+) => (\w+)" %)]
                                           [from to]
                                           %))]
    {:rules rules :molecule (first molecule)}))

(def input-rules (:rules (parse (slurp "/Users/roklenarcic/aoc/aoc15/aoc19.txt"))))
(def input-molecule (:molecule (parse (slurp "/Users/roklenarcic/aoc/aoc15/aoc19.txt"))))
(def test-rules [["H" "HO"] ["H" "OH"] ["O" "HH"] ["e" "H"] ["e" "O"]])
(def test-molecule "HOHOHO")

(defn regex-rules [rules] (re-pattern (str/join \| (distinct (map first rules)))))
(defn replacements [rules match]
  (for [[m r] rules
        :when (= m match)]
    r))

(defn p1 [rules molecule]
  (count
    (distinct
      (inputs/string-replacements
        molecule
        (regex-rules rules)
        (partial replacements rules)))))

(defn shortest-expansion [expand-fn goal-molecule molecule depth min-depth ^HashMap seen-steps]
  (if (= molecule goal-molecule)
    (do (println "Found" depth) (swap! min-depth #(min % depth)))
    (if-let [remaining (.get seen-steps molecule)]
      (swap! min-depth #(min % (+ depth remaining)))
      (if (and (< (count molecule) (count goal-molecule)) (< depth @min-depth))
        (let [min-steps (reduce min (map #(shortest-expansion expand-fn goal-molecule % (inc depth) min-depth seen-steps) (expand-fn molecule)))]
          (.put seen-steps molecule (- min-steps depth))
          min-steps)
        Long/MAX_VALUE))))

(def depth (atom Long/MAX_VALUE))

(defn shortest-reduction [rules molecule depth min-depth]
  (if (= "e" molecule)
    (swap! min-depth #(min % depth))
    (when (< depth @min-depth)
      (doseq [[re repl] rules
              next-molecule (inputs/string-replacements molecule re (constantly repl))]
        (shortest-reduction rules next-molecule (inc depth) min-depth)))))

(defn p2 [rules molecule]
  (let [inverted-rules (update-keys (->> (map reverse rules)
                                         (map vec)
                                         (into {}))
                                    re-pattern)
        _ (reset! depth (dec (count molecule)))]
    (shortest-reduction inverted-rules molecule 0 depth)
    @depth))
