(ns aoc2021.aoc16
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn hex->bin [s]
  (apply str
         (map
           {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110" \7 "0111" \8 "1000"
            \9 "1001" \A "1010" \B "1011" \C "1100" \D "1101" \E "1110" \F "1111"}
           s)))

(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc21/aoc16.txt")))


(defrecord Input [s])
(defn read-bits [in n]
  (let [s @(:s in)
        cnt (min n (count s))]
    (swap! (:s in) subs cnt)
    (subs s 0 cnt)))
(defn read-int [in bits]
  (Long/parseLong (read-bits in bits) 2))

(defn empty-input? [in]
  (-> in :s deref empty?))

(defn read-literal [in]
  (reduce (fn [acc part]
            (cond-> (str acc (subs part 1))
              (= \0 (first part)) reduced))
          ""
          (repeatedly #(read-bits in 5))))

(defn read-packet [in]
  (let [ver (read-int in 3)
        typ (read-int in 3)]
    (if (= typ 4)
      {:ver ver :typ typ :val (Long/parseLong (read-literal in) 2)}
      (if (= "0" (read-bits in 1))
        (let [subpackets-bits (read-bits in (read-int in 15))
              subpacket-input (->Input (atom subpackets-bits))]
          (loop [packets []]
            (if (empty-input? subpacket-input)
              {:ver ver :typ typ :children packets}
              (recur (conj packets (read-packet subpacket-input))))))
        {:ver ver :typ typ :children (vec (repeatedly (read-int in 11) #(read-packet in)))}))))

(defmulti op :typ)
(defmethod op 0 [{:keys [children]}] (reduce + children))
(defmethod op 1 [{:keys [children]}] (reduce * children))
(defmethod op 2 [{:keys [children]}] (reduce min children))
(defmethod op 3 [{:keys [children]}] (reduce max children))
(defmethod op 4 [packet] (:val packet))
(defmethod op 5 [{:keys [children]}] (if (> (first children) (second children)) 1 0))
(defmethod op 6 [{:keys [children]}] (if (< (first children) (second children)) 1 0))
(defmethod op 7 [{:keys [children]}] (if (= (first children) (second children)) 1 0))

(defn p1 [in]
  (let [in (->Input (atom (hex->bin in)))]
    (walk/postwalk
      (fn [x]
        (if-let [v (:ver x)]
          (apply + v (:children x)) x))
      (read-packet in))))

(defn p2 [in]
  (let [in (->Input (atom (hex->bin in)))]
    (walk/postwalk
      (fn [x] (if (:ver x) (op x) x))
      (read-packet in))))
