(ns clojure-algorithms.conversions.rectangular-to-polar
  "Port of conversions/rectangular_to_polar.py.
  Converts rectangular coordinates to polar [modulus angle-degrees].")

(defn- round2 [x]
  (/ (Math/rint (* (double x) 100.0)) 100.0))

(defn rectangular-to-polar
  "Convert rectangular (REAL, IMG) to [modulus angle-degrees]."
  [real img]
  [(round2 (Math/sqrt (+ (* real real) (* img img))))
   (round2 (Math/toDegrees (Math/atan2 (double img) (double real))))])
