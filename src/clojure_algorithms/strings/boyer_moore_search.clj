(ns clojure-algorithms.strings.boyer-moore-search
  "Boyer-Moore search with the bad-character heuristic.
  Ported from TheAlgorithms/Python.")

(defn match-in-pattern
  "Last index of `ch` in `pattern` searching from the right, or -1."
  [pattern ch]
  (let [c (if (string? ch) (first ch) ch)]
    (or (first (filter #(= c (nth pattern %))
                       (range (dec (count pattern)) -1 -1)))
        -1)))

(defn mismatch-in-text
  "Index in `text` of the mismatch against `pattern` aligned at
  `current-pos` (compared from the right), or -1 when fully matched."
  [text pattern current-pos]
  (or (first (filter #(not= (nth pattern (- % current-pos))
                            (nth text %))
                     (range (+ current-pos (dec (count pattern)))
                            (dec current-pos) -1)))
      -1))

(defn bad-character-heuristic
  "All starting positions of `pattern` in `text`."
  [text pattern]
  (let [n (count text)
        m (count pattern)]
    (loop [i 0 positions []]
      (if (> i (- n m))
        positions
        (let [mm (mismatch-in-text text pattern i)]
          (if (= mm -1)
            (recur (inc i) (conj positions i))
            (recur (max (inc i) (- mm (match-in-pattern pattern (nth text mm))))
                   positions)))))))
