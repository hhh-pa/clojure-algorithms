(ns clojure-algorithms.backtracking.match-word-pattern
  "Decide whether a pattern string matches an input string under a
  consistent (bijective) mapping from pattern chars to substrings.")

(defn- backtrack
  [pattern input-string pattern-index str-index pattern-map str-map]
  (cond
    (and (= pattern-index (count pattern)) (= str-index (count input-string)))
    true
    (or (= pattern-index (count pattern)) (= str-index (count input-string)))
    false
    :else
    (let [ch (nth pattern pattern-index)]
      (if (contains? pattern-map ch)
        (let [mapped (get pattern-map ch)]
          (if (.startsWith ^String input-string ^String mapped ^int str-index)
            (backtrack pattern input-string (inc pattern-index)
                       (+ str-index (count mapped)) pattern-map str-map)
            false))
        (some true?
              (for [end (range (inc str-index) (inc (count input-string)))
                    :let [substr (subs input-string str-index end)]
                    :when (not (contains? str-map substr))]
                (backtrack pattern input-string (inc pattern-index) end
                           (assoc pattern-map ch substr)
                           (assoc str-map substr ch))))))))

(defn match-word-pattern
  "Returns true when `pattern` matches `input-string`: each distinct
  pattern char maps to a distinct non-empty substring."
  [pattern input-string]
  (boolean (backtrack pattern input-string 0 0 {} {})))
