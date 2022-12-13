(ns aoc2020.aoc7
  (:require [clojure.string :as str]))

(defn parse-bag [l]
  (let [[_ bag contents] (re-find #"(.*) bags contain (.*)." l)
        s (re-seq #"(\d+) ((?:\w+) (?:\w+)) bags?,?" contents)]
    {:bag bag
     :contents (into {} (map (fn [[_ n bag-type]] [bag-type (parse-long n)]) s))}))

(defn parse-input [in]
  (let [parsed (->> in str/trim str/split-lines (map parse-bag))]
    (into {} (map (juxt :bag :contents)) parsed)))

(def test-input "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")
(def input (slurp "/Users/roklenarcic/aoc/aoc20/aoc7.txt"))

(defn contains-bag?
  ([m target]
   (into #{} (mapcat #(contains-bag? m % target) (keys m))))
  ([m b1 target]
   (if (= b1 target)
     [b1]
     (let [subbags (mapcat #(contains-bag? m % target) (keys (m b1)))]
       (when (seq subbags) (cons b1 subbags))))))

(defn num-of-bags
  "Count the bags inside a bag + the bag itself (so dec afterwards)"
  [m bag]
  (reduce-kv
    (fn [acc subbag cnt]
      (+ acc (* (num-of-bags m subbag) cnt)))
    1
    (m bag)))

(defn p1 [in] (dec (count (contains-bag? (parse-input in) "shiny gold"))))
(defn p2 [in] (dec (num-of-bags (parse-input in) "shiny gold")))
