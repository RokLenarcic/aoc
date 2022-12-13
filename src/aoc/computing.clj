(ns aoc.computing
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(defn parse-inst [l]
  (let [[_ f op1 op2] (re-find #"^(\w+) ([+-]?\w+)(?:,? ([+-]?\w+))?" l)
        parse-op #(or (some-> % parse-long) (keyword %))]
    (c/map-of (keyword f) (parse-op op1) (parse-op op2))))

(defn parse-prog [s] (mapv parse-inst (str/split-lines (str/trim s))))

(defn pc-invalid?
  "Returns true if pc is invalid (outside the program)"
  [state]
  (not (<= 0 (:pc state) (dec (count (:prog state))))))

(defn get-op [operand state]
  (when operand
    (if (keyword? operand) (state operand) operand)))

(defn resolve-ops
  "Resolve ops in instruction."
  [state inst]
  (-> inst (update :op1 get-op state) (update :op2 get-op state)))

(defn resolve-op1
  [state inst]
  (get-op (:op1 inst) state))

(defn resolve-op2
  [state inst]
  (get-op (:op2 inst) state))

(defn jump-op
  "A conditional jump instruction of form (fn condition, relative address). Provide predicate (fn [state op1-value])"
  [pred]
  (fn [state inst]
    (let [{:keys [op1 op2]} (resolve-ops state inst)]
      (if (pred op1)
        (update state :pc + op2 -1)
        state))))

(defn bin-op
  "A binary op where op1 op op2 and result goes into op1"
  [f]
  (fn [state inst]
    (let [{:keys [op1 op2]} (resolve-ops state inst)]
      (assoc state (:op1 inst) (f op1 op2)))))

(defn bin-op2
  "A binary op where op1 op op2 and result goes into op2"
  [f]
  (fn [state inst]
    (let [{:keys [op1 op2]} (resolve-ops state inst)]
      (assoc state (:op2 inst) (f op1 op2)))))

(defn unary-op
  "A binary op where op1 op op2 and result goes into op1"
  [f]
  (fn [state inst]
    (let [{:keys [op1]} (resolve-ops state inst)]
      (assoc state (:op1 inst) (f op1)))))

(defn run-prog
  "Runs program with initial state, instruction vector,
  map of operation to a (fn [state inst] new-state).

  State should be a map. Halt? is a (fn [state prog ops])"
  [state prog fns halt?]
  ;; init :pc if not already
  (loop [s (-> state (update :pc #(or % 0)) (update :steps #(or % 0))
               (assoc :prog prog) (assoc :fns fns))]
    (let [inst ((:prog s) (:pc s))
          op-fn ((:fns s) (:f inst))
          res (-> (op-fn s inst) (update :pc inc) (update :steps inc))]
      (if (halt? res) res (recur res)))))
