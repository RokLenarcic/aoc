(ns aoc.all-test
  (:require [clojure.test :refer :all]
            [aoc.aoc1 :as a1]
            [aoc.aoc2 :as a2]
            [aoc.aoc3 :as a3]
            [aoc.aoc4 :as a4]
            [aoc.aoc5 :as a5]
            [aoc.aoc6 :as a6]
            [aoc.aoc7 :as a7]
            [aoc.aoc8 :as a8]
            [aoc.aoc9 :as a9]
            [aoc.aoc10 :as a10]
            [aoc.aoc11 :as a11]
            [aoc.aoc12 :as a12]
            [aoc.aoc13 :as a13]
            [aoc.aoc14 :as a14]
            [aoc.aoc15 :as a15]
            [aoc.aoc16 :as a16]
            [aoc.aoc17 :as a17]
            [aoc.aoc18 :as a18]
            [aoc.aoc19 :as a19]
            [aoc.aoc20 :as a20]
            [aoc.aoc21 :as a21]
            [aoc.aoc22 :as a22]
            [aoc.aoc23 :as a23]
            [aoc.aoc24 :as a24]
            [aoc.aoc25 :as a25]))

(deftest test-solutions
  (is (= 71924 (a1/a1)))
  (is (= 210406 (a1/a2)))
  (is (= 11603 (a2/p1)))
  (is (= 12725 (a2/p2)))
  (is (= 7766 (a3/p1)))
  (is (= 2415 (a3/p2)))
  (is (= 503 (a4/p1)))
  (is (= 827 (a4/p2)))
  (is (= "TLNGFGMFN" (a5/p1)))
  (is (= "FGLQJCMBD" (a5/p2)))
  (is (= 1794 (a6/p1)))
  (is (= 2851 (a6/p2)))
  (is (= 1449447 (a7/p1)))
  (is (= 8679207 (a7/p2)))
  (is (= 1794 (a8/p1)))
  (is (= 199272 (a8/p2)))
  (is (= 5683 (a9/p 1)))
  (is (= 2372 (a9/p 9)))
  (is (= 14340 (a10/p1)))
  (is (= [nil nil nil nil nil nil nil] (a10/p2)))
  (is (= 119715 (a11/p1)))
  (is (= 18085004878 (a11/p2)))
  (is (= 412 (a12/p1 a12/input)))
  (is (= 402 (a12/p2 a12/input)))
  (is (= 6478 (a13/p1 a13/input)))
  (is (= 21922 (a13/p2 a13/input)))
  (is (= 913 (a14/p1 a14/input)))
  (is (= 30762 (a14/p2 a14/input)))
  (is (= 4876693 (a15/p1 a15/input 2000000)))
  (is (= [2911363 2855041] (a15/p2 a15/input)))
  (is (= 1820 (a16/p1 a16/input)))
  (is (= 2602 (a16/p2 a16/input)))
  (is (= 3239 (a17/p1 a17/input)))
  (is (= 1594842406882 (a17/p2 a17/input)))
  (is (= 3466 (a18/p1 a18/input)))
  (is (= 2012 (a18/p2 a18/input)))
  (is (= 2193 (a19/p1 a19/input)))
  (is (= 7200 (a19/p2 a19/input)))
  (is (= 7713 (a20/p1 a20/input)))
  (is (= 1664569352803 (a20/p2 a20/input)))
  (is (= [0 276156919469632 0] (a21/p1 a21/input)))
  (is (= 3441198826073N (a21/p2 a21/input)))
  (is (= 190066 (a22/p1 a22/input)))
  (is (= 134170 (a22/p2 a22/input)))
  (is (= 4070 (a23/p1 a23/input)))
  (is (= 881 (a23/p2 a23/input)))
  (is (= 290 (a24/p1 a24/input)))
  (is (= 842 (a24/p2 a24/input)))
  (is (= "2=12-100--1012-0=012" (a25/p1 a25/input))))
