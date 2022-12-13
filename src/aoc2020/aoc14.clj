(ns aoc2020.aoc14
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(defn gen-bitmasks [mask]
  (case (first mask)
    (\1 \0) (mapv #(str \0 %) (gen-bitmasks (rest mask)))
    \X (let [x (gen-bitmasks (rest mask))]
         (concat (mapv #(str \0 %) x)
                 (mapv #(str \1 %) x)))
    [""]))

(defn parse-line [l]
  (if-let [[_ loc v] (re-find #"mem\[(\d+)\] = (\d+)" l)]
    (c/map-of (parse-long loc) (parse-long v))
    (if-let [[_ mask] (re-find #"mask = (.*)" l)]
      {:mem-masks (mapv #(Long/parseLong % 2) (gen-bitmasks mask))
       :and-mask (Long/parseLong (str/replace mask "X" "1") 2)
       :or-mask (Long/parseLong (str/replace mask "X" "0") 2)}
      (throw (ex-info l {})))))

(defn parse-input [in]
  (let [lines (->> in str/trim str/split-lines (map parse-line))]
    (map (fn [[mask writes]]
           (assoc (first mask) :writes writes))
         (partition 2 (partition-by :or-mask lines)))))

(def test-input (parse-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"))
(def test-input2 (parse-input "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc14.txt")))

(defn run-mask-seq [m {:keys [and-mask or-mask writes]}]
  (reduce
    (fn [m' {:keys [loc v]}]
      (assoc m' loc (-> v (bit-and and-mask) (bit-or or-mask)))) m writes))

(defn compute-mem-address [mem-addr {:keys [mem-masks or-mask]}] (map #(bit-xor (bit-or mem-addr or-mask) %) mem-masks))

(defn run-mask-seq2 [m {:keys [writes] :as mask}]
  (reduce
    (fn [m' {:keys [loc v]}]
      (reduce #(assoc %1 %2 v) m' (compute-mem-address loc mask))) m writes))

(defn p1 [in]
  (c/sum-of (vals (reduce run-mask-seq {} in))))

(defn p2 [in]
  (c/sum-of (vals (reduce run-mask-seq2 {} in))))
