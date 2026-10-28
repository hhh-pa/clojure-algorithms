(ns clojure-algorithms.strings.reverse-words
  "Reverse word order in a sentence.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn reverse-words
  "Words of `sentence` in reverse order (extra whitespace ignored)."
  [sentence]
  (str/join " " (reverse (str/split sentence #"\s+"))))
