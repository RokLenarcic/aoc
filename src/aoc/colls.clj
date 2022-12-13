(ns aoc.colls
  (:require [clojure.walk :refer [walk]]
            [medley.core :as m]
            [medley.core :refer [find-first]])
  (:import (java.util HashMap)
           (java.util.function Function)))

(defn sum-of
  "Returns sum of function f over coll"
  ([coll] (reduce + coll))
  ([f coll] (reduce + (map f coll))))

(defn has? "Like contains? but looks for value, not key" [coll v]
  (boolean (m/find-first (partial = v) coll)))

(defn memoize*
  "Faster memoize not thread safe."
  [f]
  (let [mem (HashMap.)
        guard (Object.)]
    (fn [& args]
      (let [v (.getOrDefault mem args guard)]
        (if (= v guard)
          (let [ret (apply f args)]
            (.put mem args ret) ret)
          v)))))

(defn take-while2
  "Returns a transducer of successive items from coll while
  (pred acc item) returns logical true. pred must be free of side-effects.

  Return of pred is next acc."
  {:added "1.0"
   :static true}
  [pred]
  (fn [rf]
    (let [acc (volatile! nil)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if-let [next-acc (pred @acc input)]
           (do (vreset! acc next-acc)
               (rf result input))
           (reduced result)))))))

(defn- ->sym
  "Extract a symbol from the form.

  They symbol extracted will be:
  - the first symbol not in function name function call position.
  - the first keyword in function call position

  e.g. (->sym '(1 23 (inc {:a a}))) -> 'a'
       (->sym '(:x y)) -> 'x'
       (->sym '(inc y)) -> 'y'"
  [form]
  (walk ->sym
        #(if (coll? %) (find-first symbol? %) (when (symbol? %) %))
        (cond (and (list? form) (symbol? (first form))) (rest form)
              (and (list? form) (keyword? (first form))) (symbol (name (first form)))
              (map? form) (mapcat identity form)
              :else form)))

(defmacro map-of
  "Creates map with symbol names as keywords as keys and
   symbol values as values.

   Example: (map-of id name) => {:id id :name name}"
  [& syms]
  `(zipmap ~(vec (map (comp keyword ->sym) syms)) ~(vec syms)))

(defn iterate*
  "Like iterate but limited by pred, default some?"
  ([f init] (iterate* f some? init))
  ([f pred init] (take-while pred (iterate f init))))

(defn find-dedup
  "Finds value which repeats the value preceding it in sequence."
  [coll]
  (loop [c (next coll) it (first coll)]
    (let [it-next (first c)]
      (if (= it it-next) it (recur (next c) it-next)))))

(defn splits
  "Returns splits of the coll"
  [n [x & more]]
  (if more
    (let [tails (splits n more)]
      (mapcat (fn [i] (map #(update % i conj x) tails)) (range n)))
    (let [v (vec (repeat n []))]
      (map #(assoc v % [x]) (range n)))))

(defn find-cycle
  "Generic cycle finder. Small lengths will produce false positives in some cases:

  Consider: xxyyxxyyabcdefghij with min=2 and max=4 will detect xxyy as cycle. This is
  unavoidable as we don't want to examine the whole coll, otherwise this won't work on
  infinite colls.

  Returns :length :cycle-vals :prefix :prefix-vals"
  [coll min-cycle-length max-cycle-length]
  (let [c (vec (take (* 2 max-cycle-length) coll))
        r (vec (reverse c))]
    (if-let [length (m/find-first #(= (subvec r 0 %) (subvec r % (* 2 %)))
                                  (range (min max-cycle-length (quot (count c) 2)) (dec min-cycle-length) -1))]
      (let [prefix (m/find-first #(= (subvec c % (+ % length)) (subvec c (+ % length) (+ % (* 2 length))))
                                 (range length))]
        {:prefix prefix :length length
         :prefix-vals (some-> prefix (take c))
         :cycle-vals (take length (drop (or prefix 0) c))})
      (recur coll max-cycle-length (* 2 max-cycle-length)))))

(defn product
  "Produces all combinations of elements of colls. "
  [coll & more]
  (if more
    (for [tail (apply product more)
          head coll]
      (cons head tail))
    (map list coll)))

(defn count-matching
  "Counts elements matching the predicate"
  [f coll]
  (count (filter f coll)))
