(ns aoc2020.aoc23)

(defn parse-input [in]
  (mapv #(- (int %) (int \0)) in))

(defn successor-map [in]
  (assoc (zipmap in (rest in)) (last in) (first in)))

(def test-input (parse-input "389125467"))
(def input (parse-input "137826495"))

(defn look-for
  "Find dropoff point"
  [curr carry max-num]
  (let [d (dec curr)]
    (if (zero? d)
      (recur (inc max-num) carry max-num)
      (if (not-any? #(= % d) carry) d (recur d carry max-num)))))

(defn carry [succ-map curr] (take 3 (rest (iterate succ-map curr))))

(defn run-round [{:keys [succ-map curr]}]
  (let [cut-seq (carry succ-map curr)
        drop-off (look-for curr cut-seq (count succ-map))
        new-succ-map (-> succ-map
                         (assoc curr (succ-map (last cut-seq)))
                         (assoc drop-off (first cut-seq))
                         (assoc (last cut-seq) (succ-map drop-off)))]
    {:succ-map new-succ-map :curr (new-succ-map curr)}))

(defn run-rounds [in n]
  (nth (iterate run-round {:curr (first in) :succ-map (successor-map in)}) n))

(defn result-str [succ-map]
  (loop [acc "" curr 1]
    (let [x (succ-map curr)]
      (if (= x 1) acc (recur (str acc x) x)))))

(defn p1 [in] (result-str (:succ-map (run-rounds in 100))))
(defn p2 [in]
  (let [res (:succ-map (run-rounds (into in (range 10 1000001)) 10000000))
        x (res 1)
        y (res x)]
    (* x y)))
