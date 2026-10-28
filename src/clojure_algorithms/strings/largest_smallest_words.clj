(ns clojure-algorithms.strings.largest-smallest-words
  "Find the smallest and largest words in a sentence.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def ^:private strip-chars #{\. \, \! \? \( \) \[ \] \{ \}})

(defn- strip-punctuation
  [w]
  (->> w
       (drop-while strip-chars)
       reverse
       (drop-while strip-chars)
       reverse
       (apply str)))

(defn find-smallest-and-largest-words
  "Returns [smallest-word largest-word] by length in `s`,
  or [nil nil] when there are no words."
  [s]
  (let [words (filter (complement str/blank?) (str/split s #"\s+"))]
    (if (empty? words)
      [nil nil]
      (let [cleaned (mapv strip-punctuation words)]
        [(reduce (fn [a b] (if (< (count b) (count a)) b a)) cleaned)
         (reduce (fn [a b] (if (> (count b) (count a)) b a)) cleaned)]))))
