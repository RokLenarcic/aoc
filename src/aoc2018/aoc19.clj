(ns aoc2018.aoc19
  (:require [aoc2018.machine1 :as m1]
            [aoc.computing :as comp]
            [clojure.string :as str]))

(def test-input "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc19.txt")))

(defn parse-input [in]
  (let [[ip-reg & prog] (mapv m1/parse-inst (str/split-lines in))]
    {:ip-reg (:op1 ip-reg)
     :prog (vec prog)}))

(defn update-reg-pointer [state]
  (-> state (assoc :pc (get-in state [:regs (:ip-reg state)]))))

(defn execute-inst [{:keys [ip-reg] :as state} inst]
  (-> state
      (assoc-in [:regs ip-reg] (:pc state))
      (update :regs m1/oper inst)
      update-reg-pointer))

(defn execute [prog ip-reg]
  (comp/run-prog {:regs [0 0 0 0 0 0] :ip-reg ip-reg}
                 prog (constantly execute-inst) comp/pc-invalid?))

(defn p1 [in]
  (let [{:keys [prog ip-reg]} (parse-input in)]
    (get-in (execute prog ip-reg) [:regs 0])))

;;

(comment
  I0 ADDI 3 16 -> 3
  I1 SETI 1 8 -> 1
  I2 SETI 1 3 -> 4
  I3 MULR 1 4 -> 2
  I4 EQRR 2 5 -> 2
  I5 ADDR 2 3 -> 3
  I6 ADDI 3 1 -> 3
  I7 ADDR 1 0 -> 0
  I8 ADDI 4 1 -> 4
  I9 GTRR 4 5 -> 2
  I10 ADDR 3 2 -> 3
  I11 SETI 2 6 -> 3
  I12 ADDI 1 1 -> 1
  I13 GTRR 1 5 -> 2
  I14 ADDR 2 3 -> 3
  I15 SETI 1 5 -> 3
  I16 MULR 3 3 -> 3
  I17 ADDI 5 2 -> 5
  I18 MULR 5 5 -> 5
  I19 MULR 3 5 -> 5
  I20 MULI 5 11 -> 5
  I21 ADDI 2 5 -> 2
  I22 MULR 2 3 -> 2
  I23 ADDI 2 21 -> 2
  I24 ADDR 5 2 -> 5
  I25 ADDR 3 0 -> 3
  I26 SETI 0 4 -> 3
  I27 SETR 3 1 -> 2
  I28 MULR 2 3 -> 2
  I29 ADDR 3 2 -> 2
  I30 MULR 3 2 -> 2
  I31 MULI 2 14 -> 2
  I32 MULR 2 3 -> 2
  I33 ADDR 5 2 -> 5
  I34 SETI 0 3 -> 0
  I35 SETI 0 6 -> 3)

(comment
  I0 three += 16 .. JMP 17
  I1 one = 1
  I2 four = 1
  I3 two = one * four
  I4 two = two == five ? 1 :_ 0
  I5 three += two .. conditional jump to 6,7
  I6 three++ .. JMP to 8
  I7 zero += one
  I8 four++
  I9 two = four > five ? 1 :_ 0
  I10 three += two .. conditional jump to 11,12
  I11 three = 2 .. JMP to 3
  I12 one++
  I13 two = one > five ? 1 :_ 0
  I14 three += two conditional jump to 15,16
  I15 three = 1 .. JMP to 2
  I16 HALT
  I17 five += 2
  I18 five = five * five
  I19 five = 19 * five
  I20 five = five * 11
  I21 two += 5
  I22 two *= 22
  I23 two += 21
  I24 five += two
  I25 three += zero this is 0 or 1 based on init JMP to 26 .. 27
  I26 three = 0 ... JMP 1
  I27 two = 27
  I28 two *= 28
  I29 two += 29
  I30 two *= 30
  I31 two *= 14
  I32 two *= 32
  I33 five += two
  I34 zero = 0
  I35 JMP 1)

(comment
  ;; assuming [1 0 0 0 0 0] for part 2, the latter block initializes to:
  I1 [0 1 10550400 34 0 10551367]
  do [
      four = 1
      do [
          if (one * four == five) [zero += one]
          four++                                            ;
          ] (while four <= five)
      one++
      ] while (one <= five)

  ;; looks like divisor sum to me
  )

(defn p2 []
  (let [two (+ 21 (* 22 5))
        n (+ (+ two (* 11 19 2 2)) (* 30 14 32 (+ 29 (* 27 28))))]
    (->> (range 1 (inc n))
         (filter #(zero? (mod n %)))
         (reduce +))))
