(ns aoc2020.aoc20
  (:require
    [aoc.arrays :as a]
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [medley.core :as m]))

(defn parse-tile [_ [tile-header & lines]]
  (let [[_ tile-no] (re-matches #"Tile (\d+):" tile-header)
        a (for [[row-idx row] (m/indexed lines)
                [col-idx col] (m/indexed row)]
            [[row-idx col-idx] col])
        size (a/p-sum [1 1] (first (last a)))]
    [(parse-long tile-no) (map #(with-meta % {::size size}) (a/rotated-and-mirrored (into {} a)))]))

(defn parse-input [in] (into {} (inputs/blocks in :block-fn parse-tile)))

(def test-input (parse-input "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc20/aoc20.txt")))

(defn square-indices [coll]
  (let [square-size (long (Math/sqrt (count coll)))]
    (for [x (range square-size) y (range square-size)] [x y])))

(defn fits? [tile1 tile2 dir]
  (let [[rows cols] (::size (meta tile1))
        lcol (dec cols) lrow (dec rows)]
    (case dir
      :R (every? #(= (tile1 [% lcol]) (tile2 [% 0])) (range rows))
      :L (fits? tile2 tile1 :R)
      :U (fits? tile2 tile1 :D)
      :D (every? #(= (tile1 [lrow %]) (tile2 [0 %])) (range cols)))))

(defn find-fits [tile-up tile-left tiles]
  (cond-> tiles
    tile-up (update-vals (fn [coll] (filter #(fits? tile-up % :D) coll)))
    tile-left (update-vals (fn [coll] (filter #(fits? tile-left % :R) coll)))
    :always (->> (m/filter-vals seq))))

(defn construct-square
  "acc is map of pos -> [tile-num tile-map], we need both"
  [acc square-positions tiles]
  (if-let [pos (first square-positions)]
    (let [tile-up (second (acc (a/p-sum pos [-1 0])))
          tile-left (second (acc (a/p-sum pos [0 -1])))
          possibles (find-fits tile-up tile-left tiles)]
      (for [[tnum coll] possibles
            s coll
            solution (construct-square (assoc acc pos [tnum s]) (rest square-positions) (dissoc tiles tnum))]
        solution))
    [acc]))

(defn p1 [in]
  (let [sqi (square-indices in)
        ids (update-vals (first (construct-square {} sqi in)) first)
        [_ [maxx maxy]] (a/bounds sqi)]
    (reduce * (map ids [[0 0] [0 maxy] [maxx 0] [maxx maxy]]))))

(defn cut-up-and-stitch
  "Take a map of pos -> tile-map and remove tile borders and stitch together"
  [square-map]
  (let [square-size (long (Math/sqrt (count square-map)))
        ;; will be 2 smaller after the border is removed
        tile-size (long (Math/sqrt (count (val (first square-map)))))
        sm (update-vals square-map #(apply dissoc % (a/border-coordinates [tile-size tile-size])))
        tile-size (- tile-size 2)]
    (reduce merge
            (for [srow (range square-size)
                  scol (range square-size)
                  :let [tile (sm [srow scol])]]
              (update-keys tile #(a/p-sum [(dec (* srow tile-size)) (dec (* scol tile-size))] %))))))

(def seamonster
  [[1 0] [2 1] [2 4] [1 5] [1 6] [2 7] [2 10] [1 11] [1 12] [2 13] [2 16] [1 17] [1 18] [0 18] [1 19]])

(defn count-seamonsters
  [m]
  (let [seamonster? #(every? (fn [sm] (= \# (m (a/p-sum sm %)))) seamonster)]
    (c/count-matching seamonster? (square-indices m))))

(defn p2 [in]
  (let [squares (construct-square {} (square-indices in) in)
        candidates (map #(cut-up-and-stitch (update-vals % second)) squares)]
    (keep #(let [sms (count-seamonsters %)]
             (when (pos-int? sms)
               (- (count (filter (partial = \#) (vals %)))
                  (* sms (count seamonster)))))
          candidates)))
