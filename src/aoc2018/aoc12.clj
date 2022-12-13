(ns aoc2018.aoc12
  (:require [clojure.string :as str]
            [aoc.colls :refer [memoize*]]))

(def test-input "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #")
;; important property here: ..... maps to ., so no flipping infinite field
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc12.txt")))

(defn parse-rule [r]
  (let [[_ condition result] (re-find #"(.*) => (.*)" r)]
    [(mapv #(= \# %) condition) (= "#" result)]))

(defn parse-input [in]
  (let [[initial _ & more] (str/split-lines in)]
    {:pots (mapv #(= "#" %) (re-seq #"\.|#" initial))
     :rules (into {} (map parse-rule) more)
     :start-idx 0}))


(defn update-result [state new-pots]
  (let [trim-size #(reduce (fn [i x] (if x (reduced i) (inc i))) 0 %)
        start-trim (trim-size new-pots)
        end-trim (trim-size (rseq new-pots))]
    (assoc state :pots (subvec new-pots start-trim (- (count new-pots) end-trim))
                 :start-idx (+ (:start-idx state) start-trim -2))))

(def grow-pots
  (memoize*
    (fn
      [pots rules]
      (if (= 5 (count pots))
        [(rules pots false)]
        (let [lower-pots (quot (- (count pots) 4) 2)]
          (into (grow-pots (subvec pots 0 (+ 4 lower-pots)) rules)
                (grow-pots (subvec pots lower-pots) rules)))))))

(defn round [state]
  ;; 2 new pots on each side for potential pots to be full, and 2 more pots on each side to
  ;; run the patterns seamlessly
  (let [pots (-> [false false false false]
                 (into (:pots state))
                 (into [false false false false]))]
    (update-result state (grow-pots pots (:rules state)))))

(defn p1 [in]
  (let [{:keys [pots start-idx]} (nth (iterate round (parse-input in)) 20)]
    (reduce + (mapv #(if %2 %1 0) (range start-idx Long/MAX_VALUE) pots))))

(defn p2 [in]
  (let [{:keys [pots]} (nth (iterate round (parse-input in)) 89)
        start-idx (- 50000000000 88)]
    (reduce + (mapv #(if %2 %1 0) (range start-idx Long/MAX_VALUE) pots))))
