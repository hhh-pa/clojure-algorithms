(ns clojure-algorithms.electronics.electrical-impedance
  "Electrical impedance: solve for the zero-valued quantity among
  resistance, reactance and impedance. Returns a {name value} map.
  Ported from TheAlgorithms/Python electronics/electrical_impedance.py.")

(defn electrical-impedance
  "Apply Z = sqrt(R^2 + X^2); exactly one argument must be zero."
  [resistance reactance impedance]
  (when (not= 1 (count (filter zero? [resistance reactance impedance])))
    (throw (IllegalArgumentException. "One and only one argument must be 0")))
  (cond
    (zero? resistance)
    {"resistance" (Math/sqrt (- (* impedance impedance)
                                (* reactance reactance)))}
    (zero? reactance)
    {"reactance" (Math/sqrt (- (* impedance impedance)
                               (* resistance resistance)))}
    :else
    {"impedance" (Math/sqrt (+ (* resistance resistance)
                               (* reactance reactance)))}))
