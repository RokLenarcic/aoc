(ns aoc.chars)

(def char-square {:black "⬛️"
                  :green "\uD83D\uDFE9"
                  :red "\uD83D\uDFE5"
                  :blue "\uD83D\uDFE6"
                  :orange "\uD83D\uDFE7"
                  :yellow "\uD83D\uDFE8"
                  :purple "\uD83D\uDFEA"
                  :brown "\uD83D\uDFEB"})

(def char-circle {:black "\u26AB"
                  :white "\u26AA"
                  :green "\uD83D\uDFE2"
                  :red "\uD83D\uDD34"
                  :blue "\uD83D\uDD35"
                  :orange "\uD83D\uDFE0"
                  :yellow "\uD83D\uDFE1"
                  :purple "\uD83D\uDFE3"
                  :brown "\uD83D\uDFE4"})

(def shapes (merge (update-keys char-circle #(vector :circle %))
                   (update-keys char-square #(vector :square %))))
