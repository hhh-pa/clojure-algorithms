(ns clojure-algorithms.machine-learning.data-transformations
  "Normalization and standardization of numeric data.

  Normalization rescales values to [0, 1] via x_norm = (x - x_min)/(x_max - x_min).
  Standardization (z-score) rescales to mean 0 / sample-standard-deviation 1 via
  x_std = (x - mu)/sigma.

  References:
  https://en.wikipedia.org/wiki/Normalization
  https://en.wikipedia.org/wiki/Standardization")

(defn- round-to
  "Round x to ndigits decimal places with round-half-even (matches Python round)."
  [x ndigits]
  (let [scale (Math/pow 10.0 (double ndigits))]
    (/ (Math/rint (* (double x) scale)) scale)))

(defn- mean
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- sample-stdev
  "Sample standard deviation (divides by n - 1, like statistics.stdev)."
  [xs]
  (let [mu (mean xs)
        n (count xs)]
    (Math/sqrt (/ (reduce + (map #(let [d (- (double %) mu)] (* d d)) xs))
                  (dec n)))))

(defn normalization
  "Return data rescaled to [0, 1], rounded to ndigits decimal places.

  >>> (normalization [2 7 10 20 30 50])
  [0.0 0.104 0.167 0.375 0.583 1.0]"
  ([data] (normalization data 3))
  ([data ndigits]
   (let [x-min (apply min data)
         x-max (apply max data)]
     (mapv #(round-to (/ (- (double %) x-min) (- x-max x-min)) ndigits) data))))

(defn standardization
  "Return z-score standardized data, rounded to ndigits decimal places.

  >>> (standardization [5 10 15 20 25])
  [-1.265 -0.632 0.0 0.632 1.265]"
  ([data] (standardization data 3))
  ([data ndigits]
   (let [mu (mean data)
         sigma (sample-stdev data)]
     (mapv #(round-to (/ (- (double %) mu) sigma) ndigits) data))))
