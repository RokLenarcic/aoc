(ns aoc2018.aoc20
  (:require [aoc.colls :as c]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]
            [aoc.arrays :as a]))

(def test-input "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc18/aoc20.txt")))

(def parser (insta/parser "S = <'^'> cc <'$'>;
empty = eps
group = <'('> (cc | empty) ( <'|'> (cc | empty))* <')'>
cc = CC-GROUP | CC-DIR
<CC-GROUP> = CC-GROUP group | CC-DIR group | group
<CC-DIR> = CC-GROUP dirs | dirs
dirs = ('N' | 'W' | 'S' | 'E')+"))

(defmulti a "Returns [edges, path-heads]" (fn [point-map expr] (first expr)))
(defn edges [path] (mapv vector path (rest path)))

(defmethod a :S [path-heads [_ cc]]
  (into #{} (map sort) (first (a #{[0 0]} cc))))

(defmethod a :empty [path-heads _]
  [[] path-heads])

(defmethod a :group [path-heads [_ & exprs]]
  (loop [[e & more] exprs
         heads #{}
         ret-edges []]
    (let [[edges new-heads] (a path-heads e)
          new-ret-edges (into ret-edges edges)]
      (if more
        (recur more (into heads new-heads) new-ret-edges)
        [new-ret-edges (into heads new-heads)]))))

(defmethod a :cc [path-heads [_ & exprs]]
  (loop [[e & more] exprs
         heads path-heads
         ret-edges []]
    (let [[edges heads] (a heads e)
          new-ret-edges (into ret-edges edges)]
      (if more
        (recur more heads new-ret-edges)
        [new-ret-edges heads]))))

(defmethod a :dirs [path-heads [_ & ds]]
  (let [trail (map {"E" [0 1] "W" [0 -1] "N" [-1 0] "S" [1 0]} ds)
        paths (mapv #(reductions a/p-sum % trail) path-heads)]
    [(mapcat edges paths)
     (into #{} (map last) paths)]))

(defn parse-input [in] (apply u/graph (map vec (a nil (parser in)))))

(defn p1 [in]
  (let [g (parse-input in)
        paths (alg/shortest-path g {:start-node [0 0]})]
    (->> (u/nodes g)
         (apply max-key #(alg/cost-of-path (alg/path-to paths %)))
         (alg/path-to paths)
         alg/cost-of-path)))

(defn p2 [in]
  (let [g (parse-input in)
        paths (alg/shortest-path g {:start-node [0 0]})]
    (->> (u/nodes g)
         (map #(alg/cost-of-path (alg/path-to paths %)))
         (c/count-matching #(<= 1000 %)))))
