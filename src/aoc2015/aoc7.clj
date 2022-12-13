(ns aoc2015.aoc7
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]
            [medley.core :as m]
            [memento.core :as mem]
            [memento.config :as mem.c]))

(defn parse-inst [l]
  (inputs/reg-parse l
    [_ d reg] #"^(\w+) -> (\w+)" {:op :load :val d :ret reg}
    [_ x y reg] #"(\w+) OR (\w+) -> (\w+)" {:op :or :x x :y y :ret reg}
    [_ x y reg] #"(\w+) AND (\w+) -> (\w+)" {:op :and :x x :y y :ret reg}
    [_ x d reg] #"(\w+) LSHIFT (\d+) -> (\w+)" {:op :lshift :val x :d (parse-long d) :ret reg}
    [_ x d reg] #"(\w+) RSHIFT (\d+) -> (\w+)" {:op :rshift :val x :d (parse-long d) :ret reg}
    [_ d reg] #"NOT (\w+) -> (\w+)" {:op :not :val d :ret reg}))

(def test-input "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc15/aoc7.txt")))
(defn parse-prog [in]
  (->> (str/split-lines (str/trim in))
       (map parse-inst)))

(defmulti sig-val (fn [signals o]
                    (if (parse-long o) :cons (get-in signals [o :op]))))

(defmethod sig-val :cons [signals o]
  (parse-long o))
(defmethod sig-val :load [signals o]
  (let [{:keys [val _]} (signals o)]
    (sig-val signals val)))
(defmethod sig-val :or [signals o]
  (let [{:keys [x y]} (signals o)]
    (bit-or (sig-val signals x) (sig-val signals y))))
(defmethod sig-val :and [signals o]
  (let [{:keys [x y]} (signals o)]
    (bit-and (sig-val signals x) (sig-val signals y))))
(defmethod sig-val :not [signals o]
  (let [{:keys [val]} (signals o)]
    (bit-not (sig-val signals val))))
(defmethod sig-val :lshift [signals o]
  (let [{:keys [val d]} (signals o)]
    (bit-shift-left (sig-val signals val) d)))
(defmethod sig-val :rshift [signals o]
  (let [{:keys [val d]} (signals o)]
    (bit-shift-right (sig-val signals val) d)))

(mem/memo #'sig-val {mem.c/type mem.c/caffeine})
(defn p1 [in]
  (mem/memo-clear! #'sig-val)
  (let [signals (m/index-by :ret (parse-prog in))]
    (sig-val signals "a")))
(defn p2 [in]
  (mem/memo-clear! #'sig-val)
  (let [signals (assoc (m/index-by :ret (parse-prog in))
                  "b" {:op :load :val "46065"})]
    (sig-val signals "a")))
