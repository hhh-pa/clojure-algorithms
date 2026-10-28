(ns clojure-algorithms.strings.pig-latin
  "Pig Latin conversion. Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def ^:private vowels #{\a \e \i \o \u})

(defn pig-latin
  "Pig Latin form of `word` (lowercased). Blank/nil input yields \"\"."
  [word]
  (let [w (or word "")]
    (if (str/blank? w)
      ""
      (let [lower (str/lower-case w)]
        (if (contains? vowels (first lower))
          (str lower "way")
          (let [i (or (first (filter #(contains? vowels (nth lower %))
                                     (range (count lower))))
                      (count lower))]
            (str (subs lower i) (subs lower 0 i) "ay")))))))
