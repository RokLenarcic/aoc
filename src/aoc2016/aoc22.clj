(ns aoc2016.aoc22
  (:require
    [aoc.arrays :as a]
    [clojure.string :as str]
    [loom.graph :as g]
    [loom.alg :as alg]))

(defn parse-node
  [l]
  (zipmap
    [:x :y :size :used :avail]
    (map parse-long (rest (re-find #"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T" l)))))

(defn as-map [nodes]
  (zipmap (map (juxt :x :y) nodes)
          (map #(select-keys % [:size :used]) nodes)))

(def input
  (->> (slurp "/Users/roklenarcic/aoc/aoc16/aoc22.txt")
       str/trim str/split-lines
       (drop 2)
       (map parse-node)
       as-map))

(def test-input (->> "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
                     str/split-lines
                     (map parse-node)
                     as-map))

(defn transfers [in]
  (for [[k v] in
        :when (pos-int? (:used v))
        [k2 v2] in
        :when (and (not= k k2) (<= (:used v) (- (:size v2) (:used v2))))]
    [k k2]))

(defn p1 [in] (count (transfers in)))

(defn successors [{:keys [grid data empty]}]
  ;; we move the single empty space around, taking from adjacent if empty node has enough space
  (for [p2 (a/adjacent* a/dirs empty)
        :let [free-space (-> empty grid :size)
              n2 (grid p2)]
        :when (and n2 (<= (:used n2) free-space))]
    {:grid grid
     :empty p2
     :data (if (= data p2) empty data)}))


(defn p2 [in]
  (let [init-state {:grid in
                    :data [(-> (a/bounds (keys in)) second first) 0]
                    ;; only 1 node can be target of transfers
                    :empty (second (first (transfers in)))}
        g (g/fly-graph :start init-state :successors successors)]
    (some identity (alg/bf-traverse g init-state {:f #(when (= (:data %1) [0 0]) %3)}))))
