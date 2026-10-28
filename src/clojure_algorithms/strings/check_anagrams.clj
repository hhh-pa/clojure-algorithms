(ns clojure-algorithms.strings.check-anagrams
  "Case-insensitive anagram check ignoring whitespace.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn check-anagrams
  "True when `a` and `b` are anagrams (case-insensitive, spaces ignored)."
  [a b]
  (let [norm (fn [s] (sort (str/replace (str/lower-case (str/trim s))
                                        " " "")))]
    (= (norm a) (norm b))))
