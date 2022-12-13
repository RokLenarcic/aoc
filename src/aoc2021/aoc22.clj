(ns aoc2021.aoc22
  (:require [aoc.math :as m]
            [clojure.string :as str]))

(def test-input "on x=-20..26,y=-36..17,z=-47..7\non x=-20..33,y=-21..23,z=-26..28\non x=-22..28,y=-29..23,z=-38..16\non x=-46..7,y=-6..46,z=-50..-1\non x=-49..1,y=-3..46,z=-24..28\non x=2..47,y=-22..22,z=-23..27\non x=-27..23,y=-28..26,z=-21..29\non x=-39..5,y=-6..47,z=-3..44\non x=-30..21,y=-8..43,z=-13..34\non x=-22..26,y=-27..20,z=-29..19\noff x=-48..-32,y=26..41,z=-47..-37\non x=-12..35,y=6..50,z=-50..-2\noff x=-48..-32,y=-32..-16,z=-15..-5\non x=-18..26,y=-33..15,z=-7..46\noff x=-40..-22,y=-38..-28,z=23..41\non x=-16..35,y=-41..10,z=-47..6\noff x=-32..-23,y=11..30,z=-14..3\non x=-49..-5,y=-3..45,z=-29..18\noff x=18..30,y=-20..-8,z=-3..13\non x=-41..9,y=-7..43,z=-33..15\non x=-54112..-39298,y=-85059..-49293,z=-27449..7877\non x=967..23432,y=45373..81175,z=27513..53682")
(def input (slurp "/Users/roklenarcic/aoc/aoc21/aoc22.txt"))

(defn parse-cube [l]
  (let [[_ typ x-min x-max y-min y-max z-min z-max] (re-find #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" l)]
    {:typ typ
     :start [(parse-long x-min) (parse-long y-min) (parse-long z-min)]
     :end [(parse-long x-max) (parse-long y-max) (parse-long z-max)]}))

(defn add-cube
  "Add a cube, fragmenting existing cubes when needed. If cube represents on, then it's added too."
  [cubes next-cube]
  (let [next-cube' [(:start next-cube) (:end next-cube)]]
    (cond-> (mapcat #(or (second (m/intersect-shape % next-cube'))
                         [%]) cubes)
      (= "on" (:typ next-cube)) (conj next-cube'))))

(defn parse-input [in] (->> in str/split-lines (mapv parse-cube)))

(defn cube-volume [[p-min p-max]] (reduce * (map (comp inc -) p-max p-min)))

(defn p1 [in]
  (let [cubes (parse-input in)
        squashed-cubes (reduce add-cube [] cubes)
        valid-cubes (keep (partial (comp first m/intersect-shape) [[-50 -50 -50] [50 50 50]]) squashed-cubes)]
    (transduce (map cube-volume) + valid-cubes)))

(defn p2 [in]
  (let [cubes (parse-input in)]
    (transduce (map cube-volume) + (reduce add-cube [] cubes))))
