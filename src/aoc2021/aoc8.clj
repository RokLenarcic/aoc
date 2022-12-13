(ns aoc2021.aoc8
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]
    [clojure.math.combinatorics :as comb]
    [medley.core :refer [find-first]]))

(def numbers {#{\a \b \c \e \f \g} 0
              #{\c \f} 1
              #{\a \c \d \e \g} 2
              #{\a \c \d \f \g} 3
              #{\b \c \d \f} 4
              #{\a \b \d \f \g} 5
              #{\a \b \d \e \f \g} 6
              #{\a \c \f} 7
              #{\a \b \c \d \e \f \g} 8
              #{\a \b \c \d \f \g} 9})

(defn possible-wirings []
  (map zipmap
       (repeat [\a \b \c \d \e \f \g])
       (comb/permutations [\a \b \c \d \e \f \g])))

(defn real-number
  "Returns real number from scrambled input and a wiring, returns nil if not valid."
  [wiring scrambled-number]
  (numbers (into #{} (map wiring) scrambled-number)))

(defn decode-entry
  "Decodes one entry"
  [{:keys [numbers displays]}]
  (let [numbers (mapv vec numbers)
        wiring (find-first #(every? (partial real-number %) numbers) (possible-wirings))]
    (->> displays
         (mapv (partial real-number wiring))
         (apply str)
         (parse-long))))

(defn parse-entry [l]
  (let [[attempts displays] (str/split l #"\|")]
    {:numbers (str/split (str/trim attempts) #"\s+")
     :displays (str/split (str/trim displays) #"\s+")}))

(defn parse-input [in]
  (->> in str/trim str/split-lines (map parse-entry)))

(def test-input (parse-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21/aoc8.txt")))

(defn p1 [in]
  (c/count-matching #(#{2 3 4 7} (count %)) (mapcat :displays in)))

(defn p2 [in] (transduce (map decode-entry) + in))
