(ns aoc2021.aoc18
  (:require [clojure.string :as str]))

(def test-input (mapv read-string (str/split-lines "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")))
(def input (mapv read-string (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc18.txt")))))

(defn add-right-most [p x]
  (if (number? p) (+ p x) (update p 1 add-right-most x)))

(defn add-left-most [p x]
  (if (number? p) (+ p x) (update p 0 add-left-most x)))

(defn unwrap-explode [x]
  (if (map? x) 0 x))

(defn make-return-left [x y]
  (let [{:keys [left right]} (meta x)]
    (with-meta [(unwrap-explode x) (cond-> y right (add-left-most right))]
               {:left left})))

(defn make-return-right [x y]
  (let [{:keys [left right]} (meta y)]
    (with-meta [(cond-> x left (add-right-most left)) (unwrap-explode y)]
               {:right right})))

(defn explode [[x y] level]
  (if (and (number? x) (number? y))
    (when (>= level 4)
      (with-meta {} {:left x :right y}))
    (if-let [x' (when (vector? x) (explode x (inc level)))]
      (make-return-left x' y)
      (if-let [y' (when (vector? y) (explode y (inc level)))]
        (make-return-right x y')))))

(defn split [p]
  (if (number? p)
    (when (< 9 p) [(long (Math/floor (/ p 2))) (long (Math/ceil (/ p 2)))])
    (if-let [l (split (first p))]
      [l (second p)]
      (if-let [r (split (second p))]
        [(first p) r]))))

(defn sum [expr1 expr2]
  (loop [expr [expr1 expr2]]
    (if-let [ret (or (explode expr 0)
                     (split expr))]
      (recur ret)
      expr)))

(defn magnitude [expr]
  (if (number? expr) expr (+ (* 3 (magnitude (first expr)))
                             (* 2 (magnitude (second expr))))))

(defn p1 [in] (magnitude (reduce sum in)))
(defn p2 [in]
  (reduce max
          (for [x in y in :when (not= x y)]
            (magnitude (sum x y)))))
