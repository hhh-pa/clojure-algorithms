(ns clojure-algorithms.backtracking.word-ladder
  "Word Ladder: transform `begin-word` into `end-word` one letter at a time,
  each intermediate a valid word. Returns the transformation path, or []
  when none exists.
  See https://en.wikipedia.org/wiki/Word_ladder")

(def ^:private lowercase-letters (map char (range (int \a) (inc (int \z)))))

(defn backtrack
  "DFS from `current-word` over single-letter transformations found in
  `word-set` (used words are consumed). Returns the full path from the
  original begin word, or [] when the end is unreachable."
  [current-word path end-word word-set]
  (if (= current-word end-word)
    path
    (or (some (fn [[i c]]
                (let [transformed (str (subs current-word 0 i) c
                                       (subs current-word (inc i)))]
                  (when (contains? word-set transformed)
                    (let [result (backtrack transformed (conj path transformed)
                                            end-word (disj word-set transformed))]
                      (when (seq result) result)))))
              (for [i (range (count current-word))
                    c lowercase-letters]
                [i c]))
        [])))

(defn word-ladder
  "Returns the ladder from `begin-word` to `end-word` through `word-set`,
  or [] when `end-word` is absent or unreachable."
  [begin-word end-word word-set]
  (let [words (set word-set)]
    (if (not (contains? words end-word))
      []
      (backtrack begin-word [begin-word] end-word words))))
