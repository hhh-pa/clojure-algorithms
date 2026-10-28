(ns clojure-algorithms.strings.word-patterns
  "Numeric word patterns (first-seen letter indices).
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn get-word-pattern
  "Dotted pattern of `word`, e.g. \"pattern\" -> \"0.1.2.2.3.4.5\".
  Throws IllegalArgumentException for non-string input."
  [word]
  (when-not (string? word)
    (throw (IllegalArgumentException.
            (str "Expected string input, found " (type word)))))
  (let [[_ _ out]
        (reduce (fn [[nums next-n acc] ch]
                  (if (contains? nums ch)
                    [nums next-n (conj acc (get nums ch))]
                    [(assoc nums ch next-n) (inc next-n)
                     (conj acc next-n)]))
                [{} 0 []] (str/upper-case word))]
    (str/join "." out)))
