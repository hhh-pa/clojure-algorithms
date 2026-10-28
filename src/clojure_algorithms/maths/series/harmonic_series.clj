(ns clojure-algorithms.maths.series.harmonic-series
  "This is a pure Python implementation of the Harmonic Series algorithm
https://en.wikipedia.org/wiki/Harmonic_series_(mathematics)

For doctests run following command:
python -m doctest -v harmonic_series.py
or
python3 -m doctest -v harmonic_series.py

For manual testing run:
python3 harmonic_series.py"
  (:require [clojure.string :as str]))

(defn- loose-int
  [v]
  (cond (integer? v) v
        (float? v) (int v)
        (string? v) (Long/parseLong (str/trim v))
        :else (int v)))

(defn harmonic-series
  "Return the Harmonic Series from 1 to the nth term as strings."
  [n-term]
  (if (= "" n-term)
    []
    (mapv (fn [i] (if (zero? i) "1" (str "1/" (inc i))))
          (range (loose-int n-term)))))
