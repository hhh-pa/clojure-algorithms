(ns clojure-algorithms.other.guess-the-number-search
  "Binary-search guessing game (pure logic only).

  Port of TheAlgorithms/Python `other/guess_the_number_search.py`.
  The interactive `input()`-based main is intentionally not ported;
  `guess-sequence` returns the guesses instead of printing them.")

(defn get-avg
  "Mid-point (truncated) of two numbers."
  [a b]
  (int (/ (+ a b) 2)))

(defn guess-sequence
  "Return the vector of binary-search guesses that finds `to-guess`
  between `lower` and `higher` (exclusive bounds, as in the original)."
  [lower higher to-guess]
  (assert (and (integer? lower) (integer? higher) (integer? to-guess))
          "argument values must be type of \"int\"")
  (when (> lower higher)
    (throw (IllegalArgumentException.
            "argument value for lower and higher must be(lower > higher)")))
  (when (not (< lower to-guess higher))
    (throw (IllegalArgumentException.
            "guess value must be within the range of lower and higher value")))
  (loop [lo lower hi higher guesses []]
    (let [number (get-avg lo hi)
          guesses (conj guesses number)]
      (cond
        (> number to-guess) (recur lo number guesses)
        (< number to-guess) (recur number hi guesses)
        :else guesses))))

(defn guess-the-number
  "Print the guessing trace, mirroring the Python original."
  [lower higher to-guess]
  (println "started...")
  (let [guesses (guess-sequence lower higher to-guess)]
    (println (str "guess the number : " (peek guesses)))
    (println (str "details : " guesses))))
