(ns aoc2016.aoc19
  (:import (java.util ArrayList LinkedList ListIterator)))

(def input 3017957)

(defn p1 [in]
  (loop [mul 1 c 0 x in remove-even? true]
    (if (= x 1)
      (+ mul c)
      (let [x' (quot (cond-> x remove-even? inc) 2)]
        (recur (* 2 mul)
               (if remove-even? (- c mul) c)
               x'
               (= remove-even? (even? x)))))))

(defn li-next [^LinkedList l ^ListIterator li]
  (if (.hasNext li)
    (do (.next li) li)
    (doto (.listIterator l) (.next))))

(defn p2
  [in]
  (let [ll (LinkedList.)
        start-idx (int (quot in 2))]
    (dotimes [i in] (.add ll (inc i)))
    (loop [^ListIterator li (.listIterator ll start-idx)
           i in]
      (let [new-li (cond->> (doto ^ListIterator (li-next ll li)
                              (.remove))
                     (odd? i) (li-next ll))]
        (if (= 1 (.size ll)) ll (recur new-li (inc i)))))))


