(ns aoc2023.aoc3
  (:require [aoc.inputs :as in]
            [aoc.arrays :as a]
            [ubergraph.alg :as alg]))

(def test-input "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc3.txt"))

(defn parse-input [in]
  (in/map2d in #(when-not (= \. %) %)))

(defn number-char? [c] (Character/isDigit ^Character c))
(defn continuation? [m prev-p p]
  (and (= prev-p (update p 1 dec)) (number-char? (m prev-p)) (number-char? (m p))))

(defn by-row [m points]
  (->> points distinct sort
       (reduce (fn [acc p]
                 (-> (if (continuation? m (:prev acc) p)
                       (update acc :vals #(update % (dec (count %)) conj (m p)))
                       (update acc :vals conj [(m p)]))
                     (assoc :prev p)))
               {:prev nil :vals []})
       :vals))

(defn gear-ratio [components]
  (when (and (= (count components) 3) (some #(= [\*] %) components))
    (transduce (keep #(when (not= [\*] %) (parse-long (apply str %)))) * components)))

(defn p1 [in]
  (let [a (parse-input in)]
    (->> (a/array->graph (a/->MapKd a (into a/dirs a/diagonal-dirs)))
         (alg/connected-components)
         (remove (partial every? #(number-char? (a %))))
         (mapcat (partial by-row a))
         (keep #(parse-long (apply str (filter number-char? %))))
         (reduce +))))

(defn p2 [in]
  (let [a (parse-input in)]
    (->> (a/array->graph (a/->MapKd a (into a/dirs a/diagonal-dirs)))
         (alg/connected-components)
         (remove (partial every? #(number-char? (a %))))
         (map (partial by-row a))
         (keep gear-ratio)
         (reduce +))))
