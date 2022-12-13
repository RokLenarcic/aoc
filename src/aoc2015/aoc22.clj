(ns aoc2015.aoc22
  (:require [medley.core :as m]))

(def all-spells
  [{:spell :magic-missile :cost 53}
   {:spell :drain :cost 73}
   {:spell :shield :cost 113}
   {:spell :poison :cost 173}
   {:spell :recharge :cost 229}])

(defn affected? [p effect] (-> p :effects effect))
(defn consumed "Returns player with spell's mana consumed, if possible, otherwise nil" [p spell]
  (let [p* (-> p
               (update :mana - (:cost spell))
               (update :mana-spent + (:cost spell)))]
    (when (>= (:mana p*) 0) p*)))

(defmulti cast-spell
          "Tries to cast spell, returns nil if unable, otherwise returns [caster' target']"
          (fn [s caster target] (:spell s)))

(defmethod cast-spell :magic-missile
  [s caster target]
  (when-let [p* (consumed caster s)]
    [p* (update target :hp - 4)]))

(defmethod cast-spell :drain
  [s caster target]
  (when-let [p* (consumed caster s)]
    [(update p* :hp + 2) (update target :hp - 2)]))

(defmethod cast-spell :shield
  [s caster target]
  (when-let [p* (and (not (affected? caster :shield)) (consumed caster s))]
    [(update p* :effects assoc :shield 6) target]))

(defmethod cast-spell :poison
  [s caster target]
  (when-let [p* (and (not (affected? target :poison)) (consumed caster s))]
    [p* (update target :effects assoc :poison 6)]))

(defmethod cast-spell :recharge
  [s caster target]
  (when-let [p* (and (not (affected? caster :recharge)) (consumed caster s))]
    [(update p* :effects assoc :recharge 5) target]))

(defn process-effects [{:keys [effects] :as p}]
  (-> p
      (assoc :effects (m/filter-vals pos-int? (update-vals effects dec)))
      (assoc :armor (if (:shield effects) 7 0))
      (cond-> (:recharge effects) (update :mana + 101))
      (cond-> (:poison effects) (update :hp - 3))))

(def boss {:hp 71 :damage 10})
(def player {:mana 500 :mana-spent 0 :hp 50 :armor 0})

(defn finish-turn [state [p boss]]
  (assoc state :p p :boss boss :to-move (if (= :boss (:to-move state)) :p :boss)))

(defn player-turns [{:keys [p boss] :as state}]
  (let [p* (cond-> (process-effects p) (:decay? p) (update :hp dec))
        boss* (process-effects boss)]
    (if (pos-int? (:hp p*))
      (if (pos-int? (:hp boss*))
        (keep #(finish-turn state (cast-spell % p* boss*)) all-spells)
        [(finish-turn state [p* boss*])])
      [])))

(defn boss-turn [{:keys [p boss] :as state}]
  (let [p* (process-effects p)
        boss* (process-effects boss)]
    (finish-turn
      state
      (if (pos-int? (:hp boss*))
        [(update p* :hp - (- (:damage boss*) (:armor p*))) boss*]
        [p* boss*]))))

(defn fights [s]
  (if (pos-int? (-> s :p :hp))
    (if (pos-int? (-> s :boss :hp))
      (if (= :p (:to-move s))
        (let [res (keep fights (player-turns s))]
          (when (seq res) (apply min-key #(-> % :p :mana-spent) res)))
        (fights (boss-turn s)))
      s)))

(defn p1 [p boss] (fights {:p p :boss boss :to-move :p}))
  (defn p2 [p boss] (fights {:p (assoc p :decay? true) :boss boss :to-move :p}))
