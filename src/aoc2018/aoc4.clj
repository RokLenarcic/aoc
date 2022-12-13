(ns aoc2018.aoc4
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]))

(def  test-input "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up")
(def input (slurp "/Users/roklenarcic/aoc/aoc18/aoc4.txt"))

(defn parse-line [l]
  (inputs/reg-parse l
    [_ guard] #"Guard #(\d+) begins shift" {:guard (parse-long guard)
                                            :event :guard-start}
    [_ minute] #"\[[\d|-]+ 00:(\d+)\] wakes up" {:event :wakeup
                                                 :minute (parse-long minute)}
    [_ minute] #"\[[\d|-]+ 00:(\d+)\] falls asleep" {:event :asleep
                                                     :minute (parse-long minute)}))

(defn add-asleep [v minute] (merge-with + (or v (sorted-map)) (zipmap (range minute 60) (repeat 1))))
(defn add-wakeup [v minute] (merge-with - (or v (sorted-map)) (zipmap (range minute 60) (repeat 1))))

(defn parse [parsed-lines]
  (loop [ret {}
         guard-id nil
         [{:keys [event guard minute]} & more] parsed-lines]
    (case event
      nil ret
      :guard-start (recur ret guard more)
      :asleep (recur (update-in ret [guard-id] add-asleep minute)
                     guard-id
                     more)
      :wakeup (recur (update-in ret [guard-id] add-wakeup minute)
                     guard-id
                     more))))

(defn parse-input [in]
  (doall
    (for [[gid schedule] (->> (sort (str/split-lines in))
                              (map parse-line)
                              parse)
          [minute cnt] schedule]
      [gid cnt minute])))

(defn most-minutes-asleep [guard-info]
  (key (apply max-key #(->> % val (map second) (reduce +)) (group-by first guard-info))))

(defn p1 [in]
  (let [guard-info (parse-input in)
        guard-id (most-minutes-asleep guard-info)
        [guard-id _ minute] (apply max-key second (filter #(= guard-id (first %)) guard-info))]
    (* guard-id minute)))

(defn p2 [in]
  (let [guard-info (parse-input in)
        [guard-id _ minute] (apply max-key second guard-info)]
    (* guard-id minute)))
