(ns clojure-algorithms.maths.series.alternating-harmonic-series
  "Alternating Harmonic Series Checker")

(defn is-alternate-harmonic-series
  "Return true when series is an Alternating Harmonic Series, that is when
each element equals (-1)^i / (i + 1) within 1e-9."
  [series]
  (if (empty? series)
    false
    (every? (fn [[i value]]
              (let [expected (* (if (even? i) 1.0 -1.0) (/ 1.0 (inc i)))]
                (<= (Math/abs (- (double value) expected)) 1e-9)))
            (map-indexed vector series))))
