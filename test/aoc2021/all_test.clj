(ns aoc2021.all-test
  (:require [clojure.test :refer :all]
            [aoc2021.aoc1 :as a1]
            [aoc2021.aoc2 :as a2]
            [aoc2021.aoc3 :as a3]
            [aoc2021.aoc4 :as a4]
            [aoc2021.aoc5 :as a5]
            [aoc2021.aoc6 :as a6]
            [aoc2021.aoc7 :as a7]
            [aoc2021.aoc8 :as a8]
            [aoc2021.aoc9 :as a9]
            [aoc2021.aoc10 :as a10]
            [aoc2021.aoc11 :as a11]
            [aoc2021.aoc12 :as a12]
            [aoc2021.aoc13 :as a13]
            [aoc2021.aoc14 :as a14]
            [aoc2021.aoc15 :as a15]
            [aoc2021.aoc16 :as a16]
            [aoc2021.aoc17 :as a17]
            [aoc2021.aoc18 :as a18]
            [aoc2021.aoc19 :as a19]
            [aoc2021.aoc20 :as a20]
            [aoc2021.aoc21 :as a21]
            [aoc2021.aoc22 :as a22]
            [aoc2021.aoc23 :as a23]
            [aoc2021.aoc24 :as a24]
            [aoc2021.aoc25 :as a25]))

(deftest test-solutions
  (is (= 1292 (a1/p1 a1/input)))
  (is (= 1262 (a1/p2 a1/input)))
  (is (= 2019945 (a2/p1 a2/input)))
  (is (= 1599311480 (a2/p2 a2/input)))
  (is (= 2250414 (a3/p1 a3/input)))
  (is (= 6085575 (a3/p2 a3/input)))
  (is (= 41668 (a4/p1 a4/input)))
  (is (= 10478 (a4/p2 a4/input)))
  (is (= 5294 (a5/p1 a5/input)))
  (is (= 21698 (a5/p2 a5/input)))
  (is (= 390011 (a6/p1 a6/input)))
  (is (= 1746710169834 (a6/p2 a6/input)))
  (is (= 355592 (a7/p1 a7/input)))
  (is (= 101618069 (a7/p2 a7/input)))
  (is (= 554 (a8/p1 a8/input)))
  (is (= 990964 (a8/p2 a8/input)))
  (is (= 489 (a9/p1 a9/input)))
  (is (= 1056330 (a9/p2 a9/input)))
  (is (= 339411 (a10/p1 a10/input)))
  (is (= 2289754624 (a10/p2 a10/input)))
  (is (= 1669 (a11/p1 a11/input)))
  (is (= 351 (a11/p2 a11/input)))
  (is (= 4104 (a12/p1 a12/input)))
  (is (= 119760 (a12/p2 a12/input)))
  (is (= 743 (a13/p1 a13/input)))
  (is (= nil (a13/p2 a13/input)))
  (is (= 2223 (a14/p1 a14/input)))
  (is (= 2566282754493 (a14/p2 a14/input)))
  (is (= 696 (a15/p1 a15/input)))
  (is (= 2952 (a15/p2 a15/input)))
  (is (= 947 (a16/p1 a16/input)))
  (is (= 660797830937 (a16/p2 a16/input)))
  (is (= 5253 (a17/p1 a17/input)))
  (is (= 1770 (a17/p2 a17/input)))
  (is (= 4120 (a18/p1 a18/input)))
  (is (= 4725 (a18/p2 a18/input)))
  (is (= 2193 (a19/p1 a19/input)))
  (is (= 7200 (a19/p2 a19/input)))
  (is (= 5229 (a20/p1 a20/input)))
  (is (= 17009 (a20/p2 a20/input)))
  (is (= 598416 (a21/p1 a21/input)))
  (is (= [27674034218179 17242469745088] (a21/p2 a21/input)))
  (is (= 647062 (a22/p1 a22/input)))
  (is (= 1319618626668022 (a22/p2 a22/input)))
  (is (= 15109 (a23/find-path a23/input)))
  (is (= 53751 (a23/find-path a23/input-expanded)))
  (is (= "99919692496939" (a24/p1)))
  (is (= "81914111161714" (a24/p2)))
  (is (= 321 (a25/p1 a25/input))))
