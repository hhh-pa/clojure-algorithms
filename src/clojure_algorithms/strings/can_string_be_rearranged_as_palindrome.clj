(ns clojure-algorithms.strings.can-string-be-rearranged-as-palindrome
  "Checks whether a string's characters can be rearranged into a palindrome.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn can-string-be-rearranged-as-palindrome-counter
  "True when `s` (spaces dropped, case folded) can be rearranged into a
  palindrome. Frequency-map one-liner variant."
  [s]
  (< (count (filter odd? (vals (frequencies (str/lower-case
                                             (str/replace s " " ""))))))
     2))

(defn can-string-be-rearranged-as-palindrome
  "True when `s` (spaces dropped, case folded) can be rearranged into a
  palindrome. Explicit-counting variant."
  [s]
  (if (empty? s)
    true
    (let [freq (frequencies (str/lower-case (str/replace s " " "")))]
      (<= (count (filter (fn [[_ c]] (odd? c)) freq)) 1))))

(defn benchmark
  "Prints both variants' answers and timings (ms) for `s`."
  [s]
  (println "\nFor string = " s ":")
  (let [t0 (System/nanoTime)
        a (can-string-be-rearranged-as-palindrome-counter s)
        t1 (System/nanoTime)
        b (can-string-be-rearranged-as-palindrome s)
        t2 (System/nanoTime)]
    (println "> counter variant\tans =" a "\ttime =" (/ (- t1 t0) 1e6) "ms")
    (println "> explicit variant\tans =" b "\ttime =" (/ (- t2 t1) 1e6) "ms")))
