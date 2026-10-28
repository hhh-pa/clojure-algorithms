(ns clojure-algorithms.strings.remove-duplicate
  "Remove duplicate words (sorted unique words).
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn remove-duplicates
  "Sorted unique words of `sentence`, joined with single spaces."
  [sentence]
  (str/join " " (sort (set (str/split sentence #"\s+")))))
