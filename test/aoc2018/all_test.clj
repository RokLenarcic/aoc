(ns aoc2018.all-test
  (:require [clojure.test :refer :all]
            [aoc2018.aoc1 :as a1]
            [aoc2018.aoc2 :as a2]
            [aoc2018.aoc3 :as a3]
            [aoc2018.aoc4 :as a4]
            [aoc2018.aoc5 :as a5]
            [aoc2018.aoc6 :as a6]
            [aoc2018.aoc7 :as a7]
            [aoc2018.aoc8 :as a8]
            [aoc2018.aoc9 :as a9]
            [aoc2018.aoc10 :as a10]
            [aoc2018.aoc11 :as a11]
            [aoc2018.aoc12 :as a12]
            [aoc2018.aoc13 :as a13]
            [aoc2018.aoc14 :as a14]
            [aoc2018.aoc15 :as a15]
            [aoc2018.aoc16 :as a16]
            [aoc2018.aoc17 :as a17]
            [aoc2018.aoc18 :as a18]
            [aoc2018.aoc19 :as a19]
            [aoc2018.aoc20 :as a20]
            [aoc2018.aoc21 :as a21]
            [aoc2018.aoc22 :as a22]
            [aoc2018.aoc23 :as a23]
            [aoc2018.aoc24 :as a24]
            [aoc2018.aoc25 :as a25]))

(deftest test-solutions
  (is (= 518 (a1/p1 a1/input)))
  (is (= 72889 (a1/p2 a1/input)))
  (is (= 7688 (a2/p1 a2/input)))
  (is (= '("lsrivmotzbdxpkxnaqmuwcchj" "lsrivmotzbdxpkxnaqmuwcchj") (a2/p2 a2/input)))
  (is (= 111485 (a3/p1 a3/input)))
  (is (= '({:id 113 :square [[632 645] [659 664]]}) (a3/p2 a3/input)))
  (is (= 36898 (a4/p1 a4/input)))
  (is (= 80711 (a4/p2 a4/input)))
  (is (= 11894 (a5/p1 a5/input)))
  (is (= 5310 (a5/p2 a5/input)))
  (is (= 4284 (a6/p1 a6/input)))
  (is (= 35490 (a6/p2 a6/input)))
  (is (= "OVXCKZBDEHINPFSTJLUYRWGAMQ" (a7/p1 a7/input)))
  (is (= 955 (a7/p2 a7/input)))
  (is (= 37439 (a8/p1 a8/input)))
  (is (= 20815 (a8/p2 a8/input)))
  (is (= 437654 (a9/p1)))
  (is (= 3689913905 (a9/p2)))
  (is (= nil (a10/p12 a10/input)))
  (is (= [30 243 38 3] (a11/p1 a11/input)))
  (is (= [95 235 146 13] (a11/p2 a11/input)))
  (is (= 3725 (a12/p1 a12/input)))
  (is (= 3100000000293 (a12/p2 a12/input)))
  (is (= '(113 136) (a13/p1 a13/input)))
  (is (= '(114 136) (a13/p2 a13/input)))
  (is (= '(2 2 2 1 3 1 3 3 1 7) (a14/p1 030121)))
  (is (= 20287556 (a14/p2 "030121")))
  (is (= 221754 (a15/p1 a15/input)))
  (is (= 41972 (a15/p2 a15/input)))
  (is (= 517 (a16/p1 a16/input)))
  (is (= {0 667 1 667 2 3 3 2} (a16/p2 a16/input)))
  (is (= 30384 (a17/p1 a17/input)))
  (is (= 24479 (a17/p2 a17/input)))
  (is (= 427961 (a18/p1 a18/input)))
  (is (= 103970 (a18/p2 a18/input)))
  (is (= 968 (a19/p1 a19/input)))
  (is (= 10557936 (a19/p2)))
  (is (= 3574 (a20/p1 a20/input)))
  (is (= 8444 (a20/p2 a20/input)))
  (is (= 16134795 (a21/p1)))
  (is (= 14254292 (a21/p2)))
  (is (= 6318 (a22/p1 a22/input)))
  (is (= 1075 (a22/p2 a22/input)))
  (is (= 253 (a23/p1 a23/input)))
  (is (= 108618801 (a23/p2 a23/input)))
  (is (= 16086 (a24/p1 a24/input)))
  (is (= 3957 (a24/p2 a24/input)))
  (is (= 383 (a25/p1 a25/input))))
