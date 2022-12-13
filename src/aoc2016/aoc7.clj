(ns aoc2016.aoc7
  (:require
    [clojure.string :as str]
    [medley.core :as m]))

(def test-input ["abba[mnop]qrst"
                 "abcd[bddb]xyyx"
                 "aaaa[qwer]tyui"
                 "ioxxoj[asdfgh]zxcvbn"])
(def input (str/split-lines (str/trim (slurp "/Users/roklenarcic/aoc/aoc16/aoc7.txt"))))

(defn hyper-net-sequences [in]
  (map second (re-seq #"\[(\w+)\]" in)))

(defn ip-sequences [in]
  (map second (re-seq #"(?:^|\])(\w+)(?:$|\[)" in)))

(defn abba? [in]
  (m/find-first true?
                (map (fn [a1 b1 b2 a2] (and (= a1 a2) (= b1 b2) (not= b1 a1)))
                     in (next in) (drop 2 in) (drop 3 in))))

(defn aba [in]
  (map (fn [a1 b1 a2] (when (and (= a1 a2) (not= b1 a1))
                        [a1 b1]))
       in (next in) (drop 2 in)))

(defn tls? [s]
  (and (not-any? abba? (hyper-net-sequences s))
       (some abba? (ip-sequences s))))

(defn ssl? [s]
  (let [inner (mapcat aba (hyper-net-sequences s))
        outer (mapcat aba (ip-sequences s))]
    (seq
      (for [i inner
            o outer
            :when (= i (reverse o))]
        true))))

(defn p1 [in] (count (filter tls? in)))
(defn p2 [in] (count (filter ssl? in)))
