(ns clojure-algorithms.electronics.charging-capacitor
  "Capacitor voltage at time t while charging through a resistor.

  V(t) = Vs * (1 - e^(-t / (R * C))), rounded to 3 decimals.
  Ported from TheAlgorithms/Python electronics/charging_capacitor.py.")

(defn charging-capacitor
  "Capacitor voltage `time-sec` seconds after charging starts."
  [source-voltage resistance capacitance time-sec]
  (cond
    (<= source-voltage 0)
    (throw (IllegalArgumentException. "Source voltage must be positive."))
    (<= resistance 0)
    (throw (IllegalArgumentException. "Resistance must be positive."))
    (<= capacitance 0)
    (throw (IllegalArgumentException. "Capacitance must be positive."))
    :else (/ (Math/round (* 1000.0 (* source-voltage
                                      (- 1 (Math/exp (/ (- time-sec)
                                                        (* resistance capacitance)))))))
             1000.0)))
