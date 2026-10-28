(ns clojure-algorithms.electronics.charging-inductor
  "Inductor current at time t while charging through a resistor.

  I(t) = Vs/R * (1 - e^(-t*R / L)), rounded to 3 decimals.
  Ported from TheAlgorithms/Python electronics/charging_inductor.py.")

(defn charging-inductor
  "Inductor current `time` seconds after charging starts."
  [source-voltage resistance inductance time]
  (cond
    (<= source-voltage 0)
    (throw (IllegalArgumentException. "Source voltage must be positive."))
    (<= resistance 0)
    (throw (IllegalArgumentException. "Resistance must be positive."))
    (<= inductance 0)
    (throw (IllegalArgumentException. "Inductance must be positive."))
    :else (/ (Math/round (* 1000.0 (* (/ source-voltage resistance)
                                      (- 1 (Math/exp (/ (* (- time) resistance)
                                                        inductance))))))
             1000.0)))
