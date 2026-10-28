(ns clojure-algorithms.electronics.resonant-frequency
  "Resonant frequency f = 1 / (2*pi*sqrt(L*C)) of an LC circuit.
  Returns [\"Resonant frequency\" value].
  Ported from TheAlgorithms/Python electronics/resonant_frequency.py.")

(defn resonant-frequency
  "Resonant frequency from inductance and capacitance."
  [inductance capacitance]
  (cond
    (<= inductance 0)
    (throw (IllegalArgumentException. "Inductance cannot be 0 or negative"))
    (<= capacitance 0)
    (throw (IllegalArgumentException. "Capacitance cannot be 0 or negative"))
    :else ["Resonant frequency"
           (/ 1.0 (* 2 Math/PI (Math/sqrt (* inductance capacitance))))]))
