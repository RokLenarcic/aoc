(ns aoc2016.aoc17
  (:require [aoc.hash :refer [md5-hex]]))

(def closed? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a})
(def input "njfxhljp")

(defn get-valid-codes [code [row col]]
  (if (and (= row 3) (= col 3))
    [code]
    (let [h (md5-hex code)]
      (let [[u d l r] h]
        (lazy-cat
          (when-not (or (= row 0) (closed? u)) (get-valid-codes (str code \U) [(dec row) col]))
          (when-not (or (= row 3) (closed? d)) (get-valid-codes (str code \D) [(inc row) col]))
          (when-not (or (= col 0) (closed? l)) (get-valid-codes (str code \L) [row (dec col)]))
          (when-not (or (= col 3) (closed? r)) (get-valid-codes (str code \R) [row (inc col)])))))))

(defn p1 [in] (subs (apply min-key count (get-valid-codes in [0 0])) (count in)))
(defn p2 [in] (- (reduce max (map count (get-valid-codes in [0 0]))) (count in)))
