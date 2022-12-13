(ns aoc2020.aoc19
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]
    ;; cute, they think I need to code a grammar parser
            [instaparse.core :as insta]))

(defn parse-input [in]
  (inputs/blocks
    in
    :block-fn
    (fn [idx lines]
      (case idx
        0 (insta/parser (str/join \newline lines))
        1 (vec lines)))))

(def test-input "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc20/aoc19.txt")))

(defn p1 [in]
  (let [[parser inputs] (parse-input in)]
    (count (remove #(insta/failure? (insta/parse parser % :start :0)) inputs))))

(defn p2 [in]
  (p1 (-> in
          (str/replace "8: 42" "8: 42 | 42 8")
          (str/replace "11: 42 31" "11: 42 31 | 42 11 31"))))
