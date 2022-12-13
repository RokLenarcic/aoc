(ns aoc2018.aoc14
  (:import (java.util ArrayList List)))

(defn add-recipes [^List l x y]
  (let [s (+ x y)]
    (if (< s 10)
      (.add l s)
      (doto l (.add 1) (.add (mod s 10))))))

(defn advance-idx [^List l i]
  (mod (+ (inc i) (.get l i)) (.size l)))

(defn p1 [in]
  (let [l (doto (ArrayList.) (.add 3) (.add 7))]
    (loop [i 0 j 1]
      (if (< (.size l) (+ in 10))
        (do (add-recipes l (.get l i) (.get l j))
            (recur (long (advance-idx l i)) (long (advance-idx l j))))
        (seq (.subList l in (+ in 10)))))))

(defn tail-idx [^List l tail]
  (let [size (count tail)
        idx (- (dec (.size l)) size)]
    (when (< (inc size) (.size l))
      (some #(when (= tail (.subList l % (+ % size))) %)
            [idx (inc idx)]))))

(defn p2 [in]
  (let [l (doto (ArrayList.)
            (.add 3) (.add 7))
        tail (mapv (comp parse-long str) in)]
    (loop [i 0 j 1]
      (or (tail-idx l tail)
          (do (add-recipes l (.get l i) (.get l j))
              (recur (long (advance-idx l i)) (long (advance-idx l j))))))))
