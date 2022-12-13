(ns aoc2020.aoc4
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]))

(def input (slurp "/Users/roklenarcic/aoc/aoc20/aoc4.txt"))
(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

(defn create-passport [_ lines]
  (into {}
        (for [l lines
              [_ k v] (re-seq #"(\w+):(\S+)" l)]
          [(keyword k) v])))

(defn parse-input [in]
  (inputs/blocks in :block-fn create-passport))

(def fields #{:ecl :pid :iyr :hcl :eyr :byr :hgt})
(defn required-fields? [p] (>= (count (select-keys p fields)) 7))
(defn validate-year [low high] #(when-let [y (parse-long %)] (<= low y high)))
(def validators
  {:iyr (validate-year 2010 2020)
   :byr (validate-year 1920 2002)
   :eyr (validate-year 2020 2030)
   :hgt #(when-let [[_ n unit] (re-matches #"(\d+)(in|cm)" %)]
           (case unit
             "in" (<= 59 (parse-long n) 76)
             "cm" (<= 150 (parse-long n) 193)
             nil))
   :hcl #(re-matches #"#[0-9a-f]{6}" %)
   :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   :pid #(re-matches #"\d{9}" %)
   :cid any?})

(defn p1 [in] (c/count-matching required-fields? (parse-input in)))
(defn p2 [in] (c/count-matching #(and (required-fields? %)
                                      (every? (fn [[k v]] ((validators k) v)) %))
                                (parse-input in)))
