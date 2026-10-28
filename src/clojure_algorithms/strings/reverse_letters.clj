(ns clojure-algorithms.strings.reverse-letters
  "Reverse words longer than a given length.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn reverse-letters
  "Reverses every word of `sentence` longer than `length` chars."
  ([sentence] (reverse-letters sentence 0))
  ([sentence length]
   (str/join " " (map (fn [w]
                        (if (> (count w) length) (str/reverse w) w))
                      (str/split sentence #"\s+")))))
