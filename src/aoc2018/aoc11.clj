(ns aoc2018.aoc11
  (:require [aoc.colls :as c]))

(def test-input 18)
(def input 2694)

(defn power-level [id x y]
  (let [rack-id (+ x 10)]
    (-> (* rack-id (+ id (* y rack-id)))
        (quot 100)
        (mod 10)
        (- 5))))

(defn power-levels [id]
  (let [arr (object-array (repeatedly 301 #(int-array 301)))]
    (doseq [x (range 301)
          :let [arr2 (aget ^objects arr x)]
          y (range 301)]
      (aset ^ints arr2 y (int (power-level id x y))))
    arr))

(defn get-level [levels x y]
  (aget ^ints (aget ^objects levels x) y))

(def calc-square
  (c/memoize*
    (fn [levels size x y]
      (let [corner (get-level levels x y)]
        (if (= 1 size)
          corner
          (+ (reduce #(+ %1 (get-level levels (+ x %2) y)) 0 (range size))
             (reduce #(+ %1 (get-level levels x (+ y %2))) 0 (range size))
             (- corner)
             (calc-square levels (dec size) (inc x) (inc y))))))))

(defn calc-size [levels size]
  (for [x (range 1 (- 302 size))
        y (range 1 (- 302 size))]
    [(calc-square levels size x y) x y size]))

(defn p1 [in]
  (apply max-key first (calc-size (power-levels in) 3)))

(defn p2 [in]
  (let [levels (power-levels in)]
    (apply max-key first (mapcat #(calc-size levels %) (range 1 300)))))
