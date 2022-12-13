(ns aoc2020.aoc18
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(def ops {'* * '+ +})

(defn weird-precedence [op1 op2]
  (and (= '+ op1) (= '* op2)))

(defn postfix [l op]
  (conj (-> l pop pop) ((ops op) (peek l) (peek (pop l)))))

(defn shunting-yard
  "Higher precedence should return true if (op1 is higher precedence than op2)"
  [l higher-precedence?]
  (let [sht (reduce
              (fn [[output opers] x]
                (if (number? x)
                  [(conj output x) opers]
                  (loop [output' output opers' opers]
                    (if (or (empty? opers') (higher-precedence? x (peek opers')))
                      [output' (conj opers' x)]
                      (recur (postfix output' (peek opers')) (pop opers'))))))
              [[] []]
              l)]
    (first (reduce postfix (first sht) (rseq (second sht))))))

(defn reduce-expr [l higher-precedence?]
  (shunting-yard (map #(if (coll? %) (reduce-expr % higher-precedence?) %) l)
                 higher-precedence?))

(defn parse-line [in] (read-string (str "[" in "]")))

(def input (->> (slurp "/Users/roklenarcic/aoc/aoc20/aoc18.txt")
                str/trim
                str/split-lines))

(defn result [in precedence] (c/sum-of (comp #(reduce-expr % precedence) parse-line) in))

(defn p1 [in] (result in (constantly false)))
(defn p2 [in] (result in weird-precedence))
