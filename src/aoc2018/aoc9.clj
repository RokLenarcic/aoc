(ns aoc2018.aoc9
  (:import (java.util LinkedList ListIterator)))

(def test-input 25)

(defn li-next [^LinkedList l ^ListIterator li]
  (if (.hasNext li)
    (do (.next li) li)
    (doto (.listIterator l) (.next))))

(defn li-prev [^LinkedList l ^ListIterator li]
  (let [^ListIterator li (if (.hasPrevious li) li (.listIterator l (.size l)))]
    [(.previous li) li]))

(defn marble-game [players marbles]
  (let [l (doto (LinkedList.) (.add 0))]
    (reduce
      (fn [{:keys [scores li]} marble]
        (if (zero? (mod marble 23))
          (let [[item ^ListIterator li] (nth (iterate #(li-prev l (second %)) [0 li]) 8)]
            (.remove li)
            {:scores (update scores (mod marble players) (fnil + 0) item marble)
             :li (li-next l li)})
          {:scores scores
           :li (doto ^ListIterator (li-next l li)
                 (.add marble))}))
      {:scores {}
       :li (.listIterator l)}
      (range 1 (inc marbles)))))

(defn max-score [players marbles]
  (->> (marble-game players marbles) :scores vals (reduce max)))

(defn p1 []
  (max-score 400 71864))

(defn p2 []
  (max-score 400 7186400))
