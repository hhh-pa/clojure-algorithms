(ns clojure-algorithms.strings.word-occurrence
  "Word occurrence counts. Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn word-occurrence
  "Map of each word in `sentence` to its count."
  [sentence]
  (frequencies (str/split sentence #"\s+")))
