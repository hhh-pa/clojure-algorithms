(ns clojure-algorithms.strings.count-vowels
  "Count vowels in a string. Ported from TheAlgorithms/Python.")

(def ^:private vowels "aeiouAEIOU")

(defn count-vowels
  "Number of vowels in string `s`. Throws on non-string input."
  [s]
  (when-not (string? s)
    (throw (IllegalArgumentException. "Input must be a string")))
  (count (filter #(>= (.indexOf ^String vowels (int %)) 0) s)))
