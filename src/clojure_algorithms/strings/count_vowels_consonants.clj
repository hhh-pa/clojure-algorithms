(ns clojure-algorithms.strings.count-vowels-consonants
  "Count vowels and consonants, ignoring non-alphabetic characters.
  Ported from TheAlgorithms/Python.")

(def ^:private vowels #{\a \e \i \o \u \A \E \I \O \U})

(defn count-vowels-and-consonants
  "Returns [vowel-count consonant-count] for `text`."
  [text]
  (reduce (fn [[v c] ch]
            (cond
              (not (Character/isAlphabetic (int ch))) [v c]
              (contains? vowels ch) [(inc v) c]
              :else [v (inc c)]))
          [0 0] text))
