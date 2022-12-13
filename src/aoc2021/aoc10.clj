(ns aoc2021.aoc10
  (:require [clojure.string :as str]))

(def test-input (str/split-lines "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"))
(def input (str/split-lines (slurp "/Users/roklenarcic/aoc/aoc21/aoc10.txt")))

(def opening-chars {\( \) \[ \] \{ \} \< \>})

(defn parse-parenthesis
  [stack c]
  (case c
    (\( \< \{ \[) (conj stack (opening-chars c))
    (\) \> \} \]) (if (= (peek stack) c) (pop stack) (reduced c))))

(defn grade-line [l] (reduce parse-parenthesis [] l))

(defn score-completion [stack]
  (reduce (fn [score chr]
            (+ (* 5 score) ({\) 1 \] 2 \} 3 \> 4} chr)))
          0
          (reverse stack)))

(defn middle [coll] (nth coll (quot (count coll) 2)))

(defn p1 [in] (transduce (map #(get {\) 3 \} 1197 \] 57 \> 25137} % 0)) + (map grade-line in)))
(defn p2 [in] (->> (map grade-line in)
                   (filter vector?)
                   (map score-completion)
                   sort
                   middle))
