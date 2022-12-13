(ns aoc.inputs
  (:require [clojure.string :as str]
            [medley.core :as m]))

(defn parse-numbers
  ([l] (parse-numbers false l))
  ([allow-neg? l]
   (if allow-neg?
     (map parse-long (re-seq #"-?\d+" l))
     (map parse-long (re-seq #"\d+" l)))))

(defn mapped
  "If (f it) returns non-nil resp use that otherwise use the item unchanged, this is used to avoid bindings."
  [f it]
  (or (f it) it))

(defn blocks
  "Lines of text separated by a blank line. Returns seq o blocks"
  [s & {:keys [item-fn block-fn] :or {item-fn identity block-fn (fn [_ x] x)}}]
  (->> (partition-by str/blank? (str/split-lines s))
       (remove #(str/blank? (first %)))
       (keep-indexed #(block-fn %1 (map item-fn %2)))))

(defmacro reg-parse [l bind reg statement & more-clauses]
  `(if-let [~bind (re-find ~reg ~l)]
     ~statement
     ~(when (seq more-clauses)
        (apply list `reg-parse l more-clauses))))

(defn char-array2d
  [s item-xf]
  (mapv
    (fn [row]
      (mapv item-xf row))
    (str/split-lines (str/trim s))))

(defn map2d
  "Transform to map, skips values that are nil"
  [s item-xf]
  (into {}
        (for [[r-idx row] (m/indexed (str/split-lines s))
              [c-idx c] (m/indexed row)
              :let [v (item-xf c)]
              :when (some? v)]
          [[r-idx c-idx] v])))

(defn string-replacements
  "Returns list of strings where the original string has parts matching to re
  replaced with results of repl-f, where repl-f takes a match and returns a list of replacements."
  [^String s re repl-f]
  (let [m (re-matcher re s)]
    (loop [ret []]
      (if (.find m)
        (let [start (.start m)
              end (.end m)
              prefix (.subSequence s 0 start)
              suffix (.subSequence s end (count s))
              match (.group m)
              replacements (repl-f match)
              new-ret (into ret (map #(str (doto (StringBuilder.)
                                             (.append prefix)
                                             (.append ^String %)
                                             (.append suffix)))
                                     (if (coll? replacements) replacements [replacements])))]
          (recur new-ret))
        ret))))
