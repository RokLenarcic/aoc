(ns aoc2016.all-test
  (:require [clojure.test :refer :all]
            [aoc2016.aoc4 :as a4]
            [aoc2016.aoc5 :as a5]
            [aoc2016.aoc6 :as a6]
            [aoc2016.aoc7 :as a7]
            [aoc2016.aoc8 :as a8]
            [aoc2016.aoc9 :as a9]
            [aoc2016.aoc10 :as a10]
            [aoc2016.aoc11 :as a11]
            [aoc2016.aoc12 :as a12]
            [aoc2016.aoc13 :as a13]
            [aoc2016.aoc14 :as a14]
            [aoc2016.aoc15 :as a15]
            [aoc2016.aoc16 :as a16]
            [aoc2016.aoc17 :as a17]
            [aoc2016.aoc18 :as a18]
            [aoc2016.aoc19 :as a19]
            [aoc2016.aoc20 :as a20]
            [aoc2016.aoc21 :as a21]
            [aoc2016.aoc22 :as a22]
            [aoc2016.aoc23 :as a23]
            [aoc2016.aoc24 :as a24]
            [aoc2016.aoc25 :as a25]))

(deftest test-solutions
  (is (= 409147 (a4/p1 a4/input)))
  ;(is (= 827 (a4/p2 a4/input)))
  (is (= "2414bc77" (a5/p1 a5/input)))
  (is (= [\4 \3 \7 \e \6 \0 \f \c] (a5/p2 a5/input)))
  (is (= "kqsdmzft" (a6/p1 a6/input)))
  (is (= "tpooccyo" (a6/p2 a6/input)))
  (is (= 118 (a7/p1 a7/input)))
  (is (= 260 (a7/p2 a7/input)))
  (is (= 106 (a8/p1 a8/input)))
  (is (= nil (a8/p2 a8/input)))
  (is (= 123908 (a9/p1 a9/input)))
  (is (= 10755693147 (a9/p2 a9/input)))
  (is (= :bot181 (a10/p1 a10/input 61 17)))
  (is (= 12567 (a10/p2 a10/input)))
  (is (= 47 (a11/p1 a11/input)))
  (is (= 71 (a11/p2 a11/input)))
  (is (= 318020 (a12/p1 a12/input)))
  (is (= 9227674 (a12/p2 a12/input)))
  (is (= 92 (a13/p1 a13/input)))
  (is (= 124 (count (a13/p2 a13/input))))
  (is (= [18626"3b9f53b34ddd6dd60113f44a9bafca29"] (a14/p1 a14/input)))
  (is (= [20092 "0192059360a06c82e5444db43320d17a"] (a14/p2 a14/input)))
  (is (= 122318 (a15/p1 a15/input)))
  (is (= 3208583 (a15/p2 a15/input)))
  (is (= "00100111000101111" (a16/p1)))
  (is (= "11101110011100110" (a16/p2)))
  (is (= "DURLDRRDRD" (a17/p1 a17/input)))
  (is (= 650 (a17/p2 a17/input)))
  (is (= 2013 (a18/p a18/input 40)))
  (is (= 20006289 (a18/p a18/input 400000)))
  (is (= 1841611 (a19/p1 a19/input)))
  (is (= '(1423634) (a19/p2 a19/input)))
  (is (= 19449262 (a20/p1 a20/input)))
  (is (= 119.0 (a20/p2 a20/input)))
  (is (= "ghfacdbe" (a21/p1 a21/input)))
  (is (= "fhgcdaeb" (a21/p2 a21/input "fbgdceah")))
  (is (= 955 (a22/p1 a22/input)))
  (is (= 246 (a22/p2 a22/input)))
  (is (= 13140 (a23/p1 a23/input)))
  ;; another one I bruteforced
  ;(is (= 479009700 (a23/p2 a23/input)))
  (is (= 448 (a24/p1 a24/input)))
  (is (= 672 (a24/p2 a24/input)))
  (is (= 175 (a25/p1 a25/input2))))
