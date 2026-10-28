(ns clojure-algorithms.strings.is-isogram
  "Isogram check: no letter repeated.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn is-isogram
  "True when alphabetic string `s` has no repeated letter (case-insensitive).
  Throws IllegalArgumentException for non-alphabetic input."
  [s]
  (when-not (every? #(Character/isAlphabetic (int %)) s)
    (throw (IllegalArgumentException.
            "String must only contain alphabetic characters.")))
  (let [letters (str/lower-case s)]
    (= (count letters) (count (set letters)))))
