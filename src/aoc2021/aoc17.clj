(ns aoc2021.aoc17)

(def test-input "target area: x=20..30, y=-10..-5")
(def input "target area: x=265..287, y=-103..-58")

(defn new-pos [{:keys [x y velx vely]}]
  {:x (+ x velx)
   :y (+ y vely)
   :velx (cond (zero? velx) velx
               (pos-int? velx) (dec velx)
               (neg-int? velx) (inc velx))
   :vely (dec vely)})

(defn parse-input [x]
  (let [[_ minx maxx miny maxy]
        (re-find #"target area: x=([\d-]+)\.\.([\d-]+), y=([\d-]+)..([\d-]+)" x)]
    [[(parse-long minx) (parse-long miny)] [(parse-long maxx) (parse-long maxy)]]))

(defn probe-initial [velx vely] {:x 0 :y 0 :velx velx :vely vely})

(defn done? [probe [[_ miny] _]] (< (:y probe) miny))
(defn hit? [{:keys [x y]} [[minx miny] [maxx maxy]]] (and (<= minx x maxx) (<= miny y maxy)))

(defn hit-hole? [probe hole]
  (->> (iterate new-pos probe)
       (take-while #(not (done? % hole)))
       (some #(hit? % hole))))

(defn all-hit-velocities [in]
  (let [hole (parse-input in)
        max-velx (first (second hole))
        min-vely (second (first hole))
        max-vely (abs min-vely)]
    (for [velx (range (inc max-velx))
          vely (range min-vely (inc max-vely))
          :when (hit-hole? (probe-initial velx vely) hole)]
      [velx vely])))

(defn p1 [in]
  (let [best-vel (apply max (map second (all-hit-velocities in)))]
    (/ (* best-vel (inc best-vel)) 2)))

(defn p2 [in]
  (count (distinct (all-hit-velocities in))))
