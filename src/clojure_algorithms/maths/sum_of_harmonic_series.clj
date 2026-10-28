(ns clojure-algorithms.maths.sum-of-harmonic-series
  "Port of maths/sum_of_harmonic_series.py")

(defn sum-of-harmonic-progression
  "Find the sum of number-of-terms terms of a harmonic progression, via
  the arithmetic progression it is linked to.
  https://en.wikipedia.org/wiki/Harmonic_progression_(mathematics)"
  [first-term common-difference number-of-terms]
  (let [start (/ 1.0 first-term)
        arithmetic-progression
        (cons start (take (dec number-of-terms)
                          (rest (iterate #(+ % common-difference) start))))]
    (reduce + 0.0 (map #(/ 1.0 %) arithmetic-progression))))
