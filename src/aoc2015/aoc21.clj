(ns aoc2015.aoc21)

(def items
  {:armor {:leather {:cost 13 :armor 1}
              :chainmail {:cost 31 :armor 2}
              :splintmail {:cost 53 :armor 3}
              :bandedmail {:cost 75 :armor 4}
              :platemail {:cost 102 :armor 5}}
      :weapon {:dagger {:cost 8 :damage 4}
               :shortsword {:cost 10 :damage 5}
               :warhammer {:cost 25 :damage 6}
               :longsword {:cost 40 :damage 7}
               :greataxe {:cost 74 :damage 8}}
      :rings {:damage1 {:cost 25 :damage 1}
              :damage2 {:cost 50 :damage 2}
              :damage3 {:cost 100 :damage 3}
              :defense1 {:cost 20 :armor 1}
              :defense2 {:cost 40 :armor 2}
              :defense3 {:cost 80 :armor 3}}})

(def boss {:hp 103
           :damage 9
           :armor 2})

(defn player [items]
  {:hp 100
   :damage (reduce + (keep :damage items))
   :armor (reduce + (keep :armor items))})

(defn loadouts [{:keys [armor weapon rings]}]
  (let [armor-picks (cons {:cost 0} (vals armor))
        weapon-picks (vals weapon)
        left-hand-picks (cons {:cost 0 :left true} (vals rings))
        right-hand-picks (cons {:cost 0 :right true} (vals rings))]
    (for [a armor-picks
          w weapon-picks
          r1 left-hand-picks
          r2 right-hand-picks
          :when (not= r1 r2)]
      [a w r1 r2])))

(defn fight [player1 player2]
  (loop [p1 player1 p2 player2]
    (let [p1* (update p1 :hp - (max 1 (- (:damage p2) (:armor p1))))
          p2* (update p2 :hp - (max 1 (- (:damage p1) (:armor p2))))]
      (cond
        (<= (:hp p2*) 0) player1
        (<= (:hp p1*) 0) player2
        :else (recur p1* p2*)))))

(defn p1 [boss]
  (reduce min
          (for [l (loadouts items)
                :let [p (player l)]
                :when (= p (fight p boss))]
            (reduce + (map :cost l)))))

(defn p2 [boss]
  (reduce max
          (for [l (loadouts items)
                :let [p (player l)]
                :when (= boss (fight p boss))]
            (reduce + (map :cost l)))))
