(ns aoc.hash
  (:require [buddy.core.codecs :as codecs]
            [buddy.core.hash :as hash]))

(defn md5-hex [x] (codecs/bytes->hex (hash/md5 x)))

(defn md5-stream
  "N is the number of hashings."
  [secret n]
  (map #(loop [i n
               h (str secret %)]
          (if (zero? i)
            h
            (recur (dec i) (codecs/bytes->hex (hash/md5 h))))) (range)))
