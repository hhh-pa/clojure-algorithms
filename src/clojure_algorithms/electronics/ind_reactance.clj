(ns clojure-algorithms.electronics.ind-reactance
  "Inductive reactance: solve for the zero-valued quantity among
  inductance, frequency and reactance. Returns a {name value} map.
  Ported from TheAlgorithms/Python electronics/ind_reactance.py.")

(defn ind-reactance
  "Apply XL = 2*pi*f*L; exactly one argument must be zero."
  [inductance frequency reactance]
  (when (not= 1 (count (filter zero? [inductance frequency reactance])))
    (throw (IllegalArgumentException. "One and only one argument must be 0")))
  (cond
    (neg? inductance)
    (throw (IllegalArgumentException. "Inductance cannot be negative"))
    (neg? frequency)
    (throw (IllegalArgumentException. "Frequency cannot be negative"))
    (neg? reactance)
    (throw (IllegalArgumentException. "Inductive reactance cannot be negative"))
    (zero? inductance)
    {"inductance" (/ reactance (* 2 Math/PI frequency))}
    (zero? frequency)
    {"frequency" (/ reactance (* 2 Math/PI inductance))}
    :else
    {"reactance" (* 2 Math/PI frequency inductance)}))
