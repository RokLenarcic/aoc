(ns aoc2016.aoc16
  (:require [medley.core :as m]))

(defn dragon-step [a]
  (str a "0" (apply str (map {\0 \1 \1 \0} (reverse a)))))

(defn check-sum [s]
  (loop [[x y & more] s
         ret []]
    (let [ret' (conj ret (if (= x y) \1 \0))]
      (if more
        (recur more ret')
        (if (even? (count ret'))
          (recur ret' [])
          ret')))))

(def test-input "10000")
(def input "01111010110010011")

(defn checksum-for-len [in len]
  (->> (iterate dragon-step in)
       (m/find-first #(>= (count %) len))
       (take len)
       check-sum
       (apply str)))

(defn p1 [] (checksum-for-len input 272))
(defn p2 [] (checksum-for-len input 35651584))
