(ns clojure-algorithms.electronics.electric-power
  "Voltage, current or power (P = V * I) from the other two.

  Exactly one argument must be zero (the unknown); returns {:name :value}.
  Ported from TheAlgorithms/Python electronics/electric_power.py.")

(defn electric-power
  "Solve for the zero-valued quantity among voltage, current, power."
  [voltage current power]
  (when (not= 1 (count (filter zero? [voltage current power])))
    (throw (IllegalArgumentException. "Exactly one argument must be 0")))
  (when (neg? power)
    (throw (IllegalArgumentException.
            "Power cannot be negative in any electrical/electronics system")))
  (cond
    (zero? voltage) {:name "voltage" :value (double (/ power current))}
    (zero? current) {:name "current" :value (double (/ power voltage))}
    :else {:name "power"
           :value (double (/ (Math/round (* 100.0 (Math/abs (* voltage current))))
                             100.0))}))
