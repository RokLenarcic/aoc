(ns aoc2023.aoc19
  (:require [aoc.colls :as c]
            [aoc.inputs :as in]
            [clojure.string :as str]))

(def test-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}")
(def input (slurp "/Users/roklenarcic/aoc/aoc23/aoc19.txt"))

(defn parse-rule [r]
  (let [[_ dim op v dest] (re-find #"(?:(\w+)([<>])(\d+):)?(\w+)" r)]
    (if dim
      (c/map-of (keyword dim) op (parse-long v) dest)
      {:dest dest})))

(defn parse-branch [s]
  (let [[_ bname rules] (re-find #"(\w+)\{(.*)\}" s)]
    [bname (mapv parse-rule (str/split rules #","))]))

(defn parse-input [in]
  (let [[tree items] (in/blocks in)]
    {:items (mapv #(-> % (str/replace "=" " ") read-string (update-keys keyword)) items)
     :rules (into {} (map parse-branch) tree)}))

(defn valid-cube? [cube] (every? true? (mapv <= (first cube) (second cube))))

(defn rule-to-cube-mapping
  "Convert rule to two 4-dimensional cubes (one that matches and one that doesn't,
  and a dest), starting with a basis cube"
  [cube {:keys [dim op v dest]}]
  (let [idx {:x 0 :m 1 :a 2 :s 3}]
    (case op
      "<" [(assoc-in cube [1 (idx dim)] (dec v))
           (assoc-in cube [0 (idx dim)] v)
           dest]
      ">" [(assoc-in cube [0 (idx dim)] (inc v))
           (assoc-in cube [1 (idx dim)] v)
           dest]
      [cube [[0] [-1]] dest])))

(defn accepted-cubes
  ([in] (accepted-cubes (:rules (parse-input in)) [[1 1 1 1] [4000 4000 4000 4000]] "in"))
  ([all-rules cube workflow-id]
   (-> (fn [{:keys [cube ret]} rule]
         (let [[dest-cube rem-cube dest] (rule-to-cube-mapping cube rule)]
           {:cube rem-cube
            :ret (into ret (case (and (valid-cube? dest-cube) dest)
                             ("R" false) []
                             "A" [dest-cube]
                             (accepted-cubes all-rules dest-cube dest)))}))
       (reduce {:cube cube :ret []} (all-rules workflow-id))
       :ret)))

(defn cube-size [[mins maxs]] (reduce * (mapv (comp inc -) maxs mins)))

(defn p1 [in]
  (c/sum-of
    (for [[mins maxs] (accepted-cubes in)
          item (map #(mapv % [:x :m :a :s]) (:items (parse-input in)))
          :when (every? true? (mapv <= mins item maxs))]
      (reduce + item))))

(defn p2 [in] (c/sum-of cube-size (accepted-cubes in)))
