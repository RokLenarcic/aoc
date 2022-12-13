(ns aoc2015.all-test
  (:require [clojure.test :refer :all]
            [aoc2015.aoc1 :as a1]
            [aoc2015.aoc2 :as a2]
            [aoc2015.aoc3 :as a3]
            [aoc2015.aoc4 :as a4]
            [aoc2015.aoc5 :as a5]
            [aoc2015.aoc6 :as a6]
            [aoc2015.aoc7 :as a7]
            [aoc2015.aoc8 :as a8]
            [aoc2015.aoc9 :as a9]
            [aoc2015.aoc11 :as a11]
            [aoc2015.aoc12 :as a12]
            [aoc2015.aoc13 :as a13]
            [aoc2015.aoc14 :as a14]
            [aoc2015.aoc15 :as a15]
            [aoc2015.aoc16 :as a16]
            [aoc2015.aoc17 :as a17]
            [aoc2015.aoc18 :as a18]
            [aoc2015.aoc19 :as a19]
            [aoc2015.aoc20 :as a20]
            [aoc2015.aoc21 :as a21]
            [aoc2015.aoc22 :as a22]
            [aoc2015.aoc23 :as a23]
            [aoc2015.aoc24 :as a24]
            [aoc2015.aoc25 :as a25]))

(deftest test-solutions
  (is (= 138 (a1/p1 a1/input)))
  (is (= 1771 (a1/p2 a1/input)))
  (is (= 1588178 (a2/p1 a2/input)))
  (is (= 3783758 (a2/p2 a2/input)))
  (is (= 2572 (a3/p1 a3/input)))
  (is (= 2631 (a3/p2 a3/input)))
  (is (= 117946 (a4/p1 a4/input)))
  (is (= 3938038 (a4/p2 a4/input)))
  (is (= 238 (a5/p1 a5/input)))
  (is (= 69 (a5/p2 a5/input)))
  (is (= 543903 (a6/p1 a6/input)))
  (is (= 14687245 (a6/p2 a6/input)))
  (is (= 46065 (a7/p1 a7/input)))
  (is (= 14134 (a7/p2 a7/input)))
  (is (= 1371 (a8/p1 a8/input)))
  (is (= 2117 (a8/p2 a8/input)))
  (is (= 141 (a9/p1 a9/input)))
  (is (= 736 (a9/p2 a9/input)))
  (is (= "vzbxxyzz" (a11/p1 "vzbxkghb")))
  (is (= "vzcaabcc" (a11/p1 (a11/p1 "vzbxkghb"))))
  (is (= 111754 (a12/p1 a12/input)))
  (is (= 65402 (a12/p2 a12/input)))
  (is (= 664 (a13/p1 a13/input)))
  (is (= 640 (a13/p2 a13/input)))
  (is (= 2696 (a14/p1 a14/input)))
  (is (= {"Comet" 121 "Cupid" 839 "Dancer" 199 "Donner" 277 "Prancer" 24 "Rudolph" 1084 "Vixen" 13} (a14/p2 a14/input)))
  (is (= 18965440 (a15/p1 a15/input)))
  (is (= 15862900 (a15/p2 a15/input)))
  (is (= {"40" {"akitas" 0
                "cats" 7
                "vizslas" 0}} (a16/p1 a16/input)))
  (is (= {"241" {"cars" 2
                 "pomeranians" 1
                 "samoyeds" 2}} (a16/p2 a16/input)))
  (is (= 402 (a17/p1 a17/input)))
  (is (= 40 (a17/p2 a17/input)))
  (is (= 821 (a18/p1 a18/input)))
  (is (= 886 (a18/p2 a18/input)))
  (is (= 535 (a19/p1 a19/input-rules a19/input-molecule)))
  (is (= 5 (a19/p2 a19/test-rules a19/test-molecule)))
  (is (= 776160 (a20/p1 33100000)))
  (is (= 786240 (a20/p2 33100000)))
  (is (= 121 (a21/p1 a21/boss)))
  (is (= 201 (a21/p2 a21/boss)))
  (is (= {:boss {:armor 0
                 :damage 10
                 :effects {:poison 3}
                 :hp 0}
          :p {:armor 0
              :effects {}
              :hp 3
              :mana 191
              :mana-spent 1824}
          :to-move :p} (a22/p1 a22/player a22/boss)))
  (is (= {:boss {:armor 0
                 :damage 10
                 :effects {:poison 3}
                 :hp 0}
          :p {:armor 0
              :decay? true
              :effects {}
              :hp 1
              :mana 78
              :mana-spent 1937}
          :to-move :p} (a22/p2 a22/player a22/boss)))
  (is (= 184 (a23/p1 a23/input)))
  (is (= 231 (a23/p2 a23/input)))
  (is (= 11266889531 (a24/p1 a24/input)))
  (is (= 77387711 (a24/p2 a24/input)))
  (is (= 2650453 (a25/p1 a25/input))))
