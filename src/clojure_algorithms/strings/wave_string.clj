(ns clojure-algorithms.strings.wave-string
  "Mexican-wave versions of a string. Ported from TheAlgorithms/Python.")

(defn wave
  "Vector of copies of `s` with each alphabetic char upper-cased in turn."
  [s]
  (vec (keep-indexed (fn [i ch]
                       (when (Character/isAlphabetic (int ch))
                         (str (subs s 0 i)
                              (Character/toUpperCase ^char ch)
                              (subs s (inc i)))))
                     s)))
