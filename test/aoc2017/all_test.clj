(ns aoc2017.all-test
  (:require [clojure.test :refer :all]
            [aoc2017.aoc21 :as a21]
            [aoc2017.aoc22 :as a22]
            [aoc2017.aoc23 :as a23]
            [aoc2017.aoc24 :as a24]
            [aoc2017.aoc25 :as a25]))

(deftest test-solutions
  (is (= 147 (a21/p1 a21/input)))
  (is (= 1936582 (a21/p2 a21/input)))
  (is (= 5352 (a22/p1 a22/input)))
  (is (= 2511475 (a22/p2 a22/input)))
  (is (= 6241 (a23/p1 a23/input)))
  (is (= 909 (a23/p2 a23/input)))
  (is (= 1511 (a24/p1 a24/input)))
  (is (= [31 1471] (a24/p2 a24/input)))
  (is (= 4225 (a25/p1 a25/input))))
