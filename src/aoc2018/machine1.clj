(ns aoc2018.machine1
  (:require [aoc.colls :as c]))

(def opcodes [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr])

(defn oper [state {:keys [f op1 op2 dest]}]
  (let [rop1 (get state op1)
        rop2 (get state op2)]
    (assoc state
      dest
      (case f
        :addr (+ rop1 rop2)
        :addi (+ rop1 op2)
        :mulr (* rop1 rop2)
        :muli (* rop1 op2)
        :banr (bit-and rop1 rop2)
        :bani (bit-and rop1 op2)
        :borr (bit-or rop1 rop2)
        :bori (bit-or rop1 op2)
        :setr rop1
        :seti op1
        :gtir (if (> op1 rop2) 1 0)
        :gtri (if (> rop1 op2) 1 0)
        :gtrr (if (> rop1 rop2) 1 0)
        :eqir (if (= op1 rop2) 1 0)
        :eqri (if (= rop1 op2) 1 0)
        :eqrr (if (= rop1 rop2) 1 0)))))

(defn parse-inst [l]
  (let [[_ f op1 op2 dest] (re-find #"^([\w#]+) ([+-]?\w++) ?([+-]?\w++)? ?([+-]?\w++)?" l)
        parse-op #(or (some-> % parse-long) (keyword %))]
    (c/map-of (keyword f) (parse-op op1) (parse-op op2) (parse-op dest))))
