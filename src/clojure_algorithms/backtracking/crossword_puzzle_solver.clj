(ns clojure-algorithms.backtracking.crossword-puzzle-solver
  "Crossword puzzle solver via backtracking. Words are tried longest-first;
  intersections (shared letters) are supported.
  Ref: https://www.geeksforgeeks.org/solve-crossword-puzzle/
  Unlike the Python reference, which mutates the puzzle in place, all
  functions here are pure: the puzzle is a vector of vectors of strings
  (\"\" marks an empty cell).")

(defn valid?
  "Returns true when `word` can be placed at (`row`, `col`), going down
  when `vertical?` is true, right otherwise. A cell is usable when empty
  or already holding the needed letter."
  [puzzle word row col vertical?]
  (let [rows (count puzzle)
        cols (count (first puzzle))]
    (every? true?
            (map-indexed
             (fn [i ch]
               (let [r (if vertical? (+ row i) row)
                     c (if vertical? col (+ col i))]
                 (and (< r rows) (< c cols)
                      (contains? #{"" (str ch)} (get-in puzzle [r c])))))
             word))))

(defn place-word
  "Returns the puzzle with `word` written at (`row`, `col`)."
  [puzzle word row col vertical?]
  (reduce (fn [p [i ch]]
            (assoc-in p [(if vertical? (+ row i) row)
                         (if vertical? col (+ col i))]
                      (str ch)))
          puzzle
          (map-indexed vector word)))

(defn remove-word
  "Returns the puzzle with `word` removed from (`row`, `col`), restoring
  only cells that were empty in `snapshot` (crossing letters survive)."
  [puzzle word row col vertical? snapshot]
  (reduce (fn [p i]
            (let [r (if vertical? (+ row i) row)
                  c (if vertical? col (+ col i))]
              (if (= "" (get-in snapshot [r c]))
                (assoc-in p [r c] "")
                p)))
          puzzle
          (range (count word))))

(defn solve-crossword
  "Tries to fit `words` into `puzzle`. Returns the solved puzzle, or nil
  when no placement exists."
  [puzzle words]
  (if (empty? words)
    puzzle
    (let [ordered (sort-by count > words)
          word (first ordered)
          rest-words (rest ordered)]
      (some (fn [[row col vertical?]]
              (when (valid? puzzle word row col vertical?)
                (let [placed (place-word puzzle word row col vertical?)]
                  (solve-crossword placed rest-words))))
            (for [row (range (count puzzle))
                  col (range (count (first puzzle)))
                  vertical? [true false]]
              [row col vertical?])))))
