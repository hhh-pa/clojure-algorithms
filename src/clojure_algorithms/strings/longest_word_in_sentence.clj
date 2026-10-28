(ns clojure-algorithms.strings.longest-word-in-sentence
  "Longest word in a sentence. Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn longest-word
  "Longest word in `sentence`, or \"\" when there are no words."
  [sentence]
  (let [words (filter (complement str/blank?) (str/split sentence #"\s+"))]
    (if (empty? words) "" (apply max-key count words))))
