(ns aoc2018.aoc5
  (:require [clojure.string :as str]
            [aoc.colls :as c]))

(def test-input "dabAcCaCBAcCcaDA")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc5.txt")))
(def alph "ABCDEFGHIJKLMNOQPRSTUVWXYZ")
(def reagents (merge (zipmap alph (str/lower-case alph))
                     (zipmap (str/lower-case alph) alph)))

(defn react [in]
  (loop [[x y & more] in
         ret []]
    (if x
      (if y
        (if (= x (reagents y))
          (recur more ret)
          (recur (cons y more) (conj ret x)))
        (conj ret x))
      ret)))

(defn reduced-length [in] (count (c/find-dedup (iterate react in))))

(defn p1 [in] (reduced-length in))
(defn p2 [in]
  (reduce min
          (for [ch alph]
            (reduced-length (str/replace in (re-pattern (str ch "|" (Character/toLowerCase ^Character ch))) "")))))
