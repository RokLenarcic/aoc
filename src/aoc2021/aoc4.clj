(ns aoc2021.aoc4
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [medley.core :as m]))

(def combos (concat (map #(map (fn [x] [% x]) (range 5)) (range 5))
                    (map #(map (fn [x] [x %]) (range 5)) (range 5))))

(defn parse-board [lines]
  (into {}
        (for [[row-idx row] (m/indexed lines)
              [col-idx n] (m/indexed (inputs/parse-numbers row))]
          [n [row-idx col-idx]])))

(defn parse-input [in]
  (apply
    merge-with
    into
    (inputs/blocks in :block-fn (fn [idx lines]
                                  (if (zero? idx)
                                    {:numbers (inputs/parse-numbers (first lines))}
                                    {:boards [(parse-board lines)]})))))

(def test-input (parse-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21/aoc4.txt")))

(defn winning-boards [numbers boards]
  (for [b boards
        :let [hits (into #{} (map b numbers))]
        combo combos
        :when (every? hits combo)]
    b))

(defn score [board numbers] (* (last numbers) (c/sum-of (keys (apply dissoc board numbers)))))
(defn find-a-winner [{:keys [numbers boards]}]
  (loop [[ns & more] (reductions conj [] numbers)]
    (if-let [board (first (winning-boards ns boards))]
      [board ns]
      (when more (recur more)))))

(defn p1 [in] (apply score (find-a-winner in)))
(defn p2 [in]
  (loop [{:keys [numbers boards] :as in} in
         results []]
    (if-let [[b ns] (find-a-winner in)]
      (recur {:boards (remove #(= b %) boards)
              :numbers numbers}
             (conj results (score b ns)))
      (last results))))
