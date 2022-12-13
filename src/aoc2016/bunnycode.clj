(ns aoc2016.bunnycode
  (:require [aoc.computing :as c]))

(defn init-state []
  {:a 0 :b 0 :c 0 :d 0})

(defn toggle-command [prog i]
  (if-let [{:keys [f] :as inst} (get prog i)]
    (assoc prog i (update inst :f #(case %
                                     :inc :dec
                                     (:dec :tgl) :inc
                                     :jnz :cpy
                                     :cpy :jnz)))
    prog))

(def opcodes
  {:cpy (fn [state {:keys [op2] :as inst}]
          (if (number? op2)
            state                                           ; skip nonsense
            (assoc state op2 (c/resolve-op1 state inst))))
   :out (fn [state inst]
          (update state :out (fnil conj []) (c/resolve-op1 state inst)))
   :inc (c/unary-op inc)
   :dec (c/unary-op dec)
   :jnz (c/jump-op #(not= % 0))
   :tgl (fn [state inst]
          (update state :prog toggle-command (+ (:pc state) (c/resolve-op1 state inst))))})

(defn run-bunnycode [in state-update]
  (let [p (c/parse-prog in)]
    (c/run-prog (merge (init-state) state-update) p opcodes c/pc-invalid?)))
