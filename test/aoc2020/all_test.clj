(ns aoc2020.all-test
  (:require [clojure.test :refer :all]
            [aoc2020.aoc3 :as a3]
            [aoc2020.aoc4 :as a4]
            [aoc2020.aoc5 :as a5]
            [aoc2020.aoc6 :as a6]
            [aoc2020.aoc7 :as a7]
            [aoc2020.aoc8 :as a8]
            [aoc2020.aoc9 :as a9]
            [aoc2020.aoc10 :as a10]
            [aoc2020.aoc11 :as a11]
            [aoc2020.aoc12 :as a12]
            [aoc2020.aoc13 :as a13]
            [aoc2020.aoc14 :as a14]
            [aoc2020.aoc15 :as a15]
            [aoc2020.aoc16 :as a16]
            [aoc2020.aoc17 :as a17]
            [aoc2020.aoc18 :as a18]
            [aoc2020.aoc19 :as a19]
            [aoc2020.aoc20 :as a20]
            [aoc2020.aoc21 :as a21]
            [aoc2020.aoc22 :as a22]
            [aoc2020.aoc23 :as a23]
            [aoc2020.aoc24 :as a24]
            [aoc2020.aoc25 :as a25]))

(deftest test-solutions
  (is (= 244 (a3/p1 a3/input)))
  (is (= 9406609920 (a3/p2 a3/input)))
  (is (= 219 (a4/p1 a4/input)))
  (is (= 127 (a4/p2 a4/input)))
  (is (= 938 (a5/p1 a5/input)))
  (is (= '(696) (a5/p2 a5/input)))
  (is (= 6714 (a6/p1 a6/input)))
  (is (= 3435 (a6/p2 a6/input)))
  (is (= 128 (a7/p1 a7/input)))
  (is (= 20189 (a7/p2 a7/input)))
  (is (= 1384 (a8/p1 a8/input)))
  (is (= 761 (a8/p2 a8/input)))
  (is (= 530627549 (a9/p1 a9/input 25)))
  (is (= 77730285 (a9/p2 a9/input 530627549)))
  (is (= 2664 (a10/p1 a10/input)))
  (is (= 148098383347712 (a10/p2 a10/input)))
  (is (= 2412 (a11/p1 a11/input)))
  (is (= 2176 (a11/p2 a11/input)))
  (is (= 1645 (a12/p1 a12/input)))
  (is (= 35292 (a12/p2 a12/input)))
  (is (= 171 (a13/p1 a13/input)))
  (is (= 539746751134958 (a13/p2 a13/input)))
  (is (= 14553106347726 (a14/p1 a14/input)))
  (is (= 2737766154126 (a14/p2 a14/input)))
  (is (= 441 (a15/p1 a15/input)))
  (is (= 10613991 (a15/p2 a15/input)))
  (is (= 20975 (a16/p1 a16/input)))
  (is (= 910339449193 (a16/p2 a16/input)))
  (is (= 388 (a17/p1 a17/input)))
  (is (= 2280 (a17/p2 a17/input)))
  (is (= 15285807527593 (a18/p1 a18/input)))
  (is (= 461295257566346 (a18/p2 a18/input)))
  (is (= 102 (a19/p1 a19/input)))
  (is (= 318 (a19/p2 a19/input)))
  (is (= 108603771107737 (a20/p1 a20/input)))
  (is (= '(2129) (a20/p2 a20/input)))
  (is (= 2265 (a21/p1 a21/input)))
  (is (= "dtb,zgk,pxr,cqnl,xkclg,xtzh,jpnv,lsvlx" (a21/p2 a21/input)))
  (is (= 34324 (a22/p1 a22/input)))
  (is (= 33259 (a22/p2 a22/input)))
  (is (= "59374826" (a23/p1 a23/input)))
  (is (= 66878091588 (a23/p2 a23/input)))
  (is (= 373 (a24/p1 a24/input)))
  (is (= 3917 (a24/p2 a24/input)))
  (is (= 12181021 (a25/p1 a25/input))))
