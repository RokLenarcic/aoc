(ns aoc2018.aoc21
  (:require [aoc2018.machine1 :as m1]
            [aoc.computing :as comp]
            [clojure.string :as str]
            [flatland.ordered.set :as oset]))

(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc21.txt")))
;;

(comment
  I0 SETI 123 0 -> 3
  I1 BANI 3 456 -> 3
  I2 EQRI 3 72 -> 3
  I3 ADDR 3 2 -> 2
  I4 SETI 0 0 -> 2
  I5 SETI 0 4 -> 3
  I6 BORI 3 65536 -> 4
  I7 SETI 1107552 3 -> 3
  I8 BANI 4 255 -> 5
  I9 ADDR 3 5 -> 3
  I10 BANI 3 16777215 -> 3
  I11 MULI 3 65899 -> 3
  I12 BANI 3 16777215 -> 3
  I13 GTIR 256 4 -> 5
  I14 ADDR 5 2 -> 2
  I15 ADDI 2 1 -> 2
  I16 SETI 27 0 -> 2
  I17 SETI 0 2 -> 5
  I18 ADDI 5 1 -> 1
  I19 MULI 1 256 -> 1
  I20 GTRR 1 4 -> 1
  I21 ADDR 1 2 -> 2
  I22 ADDI 2 1 -> 2
  I23 SETI 25 3 -> 2
  I24 ADDI 5 1 -> 5
  I25 SETI 17 3 -> 2
  I26 SETR 5 3 -> 4
  I27 SETI 7 4 -> 2
  I28 EQRR 3 0 -> 5
  I29 ADDR 5 2 -> 2
  I30 SETI 5 8 -> 2)

(comment
  I0 three = 123
  I1 three &= 456
  I2 three = three == 72 ? 1 :_ 0
  I3 two += three
  I4 two = 0
  I5 three = 0
  I6 four = three | 65536
  I7 three = 1107552
  I8 five = four & 255
  I9 three += five
  I10 three &= 16777215
  I11 three *= 65899
  I12 three &= 16777215
  I13 five = 256 > four ? 1 :_ 0
  I14 two += five
  I15 two += 1
  I16 two = 27
  I17 five = 0
  I18 one = five + 1
  I19 one *= 256
  I20 one = one > four ? 1 :_ 0
  I21 two += one
  I22 two += 1
  I23 two = 25
  I24 five += 1
  I25 two = 17
  I26 four = five
  I27 two = 7
  I28 five = three == zero ? 1 :_ 0
  I29 two += five
  I30 two = 5)

(comment
  I0 three = 123
  I1 three &= 456
  I2 three = three == 72 ? 1 :_ 0
  I3 CJMP 4, 5
  I4 JMP 1
  I5 three = 0
  I6 four = three | 65536
  I7 three = 1107552
  I8 five = four & 255
  I9 three += five
  I10 three &= 16777215
  I11 three *= 65899
  I12 three &= 16777215
  I13 five = 256 > four ? 1 :_ 0
  I14 CJMP five 15, 16
  I15 JMP 17
  I16 JMP 28
  I17 five = 0
  I18 one = five + 1
  I19 one *= 256
  I20 one = one > four ? 1 :_ 0
  I21 CJMP one 22, 23
  I22 JMP 24
  I23 JMP 26
  I24 five += 1
  I25 JMP 18
  I26 four = five
  I27 JMP 8
  I28 five = three == zero ? 1 :_ 0
  I29 CJMP 30, 31
  I30 JMP 6)





(comment
  do [four = three | 0xffff
      three = 1107552
      do [five = four & 0xff
          three += five
          three &= 0xffffff
          three *= 65899
          three &= 0xffffff
          if (256 > four) break
          for (five = 0, one <= four, five++)
              [one = 256 * (five + 1)]
          four = five
         ] while (true)
      ] (while (three != zero)))

(comment
  do [four = three | 0xffff
      three = 1107552
      do [three += four & 0xff
          three &= 0xffffff
          three *= 65899
          three &= 0xffffff
          four = four quot 256
          ] while (four != 0)
      ] (while (three != zero)))

(comment
  do [four = three | 0xffff
      three = 1107552
      do [three += four & 0xff
          three &= 0xffffff
          three *= 65899
          three &= 0xffffff
          four = four quot 256
          ] while (four != 0)
      ] (while (three != zero)))

(defn inner-prog-loop [x]
  (loop [four (bit-or x 0x10000)
         three 1107552]
    (let [three (-> three
                    (+ (bit-and four 0xff))
                    (bit-and 0xffffff)
                    (* 65899)
                    (bit-and 0xffffff))
          four (quot four 256)]
      (if (zero? four) three (recur four three)))))

;; program analysis yields this solution
(defn p1 [] (second (iterate inner-prog-loop 0)))
(defn p2 []
  (reduce (fn [seen i] (if (seen i) (reduced (last seen)) (conj seen i)))
          (oset/ordered-set)
          (iterate inner-prog-loop 0)))
