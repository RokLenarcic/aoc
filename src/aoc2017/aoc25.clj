(ns aoc2017.aoc25
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]))

(def test-input "Begin in state A.\nPerform a diagnostic checksum after 6 steps.\n\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\n\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A.")
(def input (slurp "/Users/roklenarcic/aoc/aoc17/aoc25.txt"))

(defn parse-state-change [state-desc]
  (let [[_ s] (re-find #"In state (\w+):" state-desc)
        write (map second (re-seq #"- Write the value (\d+)" state-desc))
        move (map second (re-seq #"- Move one slot to the (\w+)" state-desc))
        continues (map second (re-seq #"- Continue with state (\w+)" state-desc))]
    [(keyword s) {0 {:write (parse-long (first write))
                     :move (keyword (first move))
                     :jmp (keyword (first continues))}
                  1 {:write (parse-long (second write))
                     :move (keyword (second move))
                     :jmp (keyword (second continues))}}]))

(defn parse-input
  [in]
  (let [[start & state-changes] (str/split in #"\n\n")
        [_ state check-sum] (re-find #"Begin in state (\w+)\.\nPerform a diagnostic checksum after (\d+) steps." start)
        xf (into {} (map parse-state-change state-changes))]
    (c/map-of (keyword state) (parse-long check-sum) xf)))

(defn action [{:keys [state tape pos xf]}]
  (get-in xf [state (tape pos 0)]))

(defn do-action [{:keys [pos] :as machine} {:keys [write move jmp] :as a}]
  (-> machine
      (assoc-in [:tape pos] write)
      (assoc :state jmp)
      (assoc :pos (if (= :left move) (dec pos) (inc pos)))
      (update :cnt inc)))

(defn p1 [in]
  (loop [{:keys [tape cnt check-sum] :as machine} (assoc (parse-input in) :pos 0 :cnt 0 :tape {})]
    (if (= cnt check-sum)
      (count (filter #(= 1 %) (vals tape)))
      (recur (do-action machine (action machine))))))
