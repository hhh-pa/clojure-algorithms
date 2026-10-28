(ns clojure-algorithms.electronics.ic-555-timer
  "Frequency and duty cycle of an astable 555 timer.
  Ported from TheAlgorithms/Python electronics/ic_555_timer.py.")

(defn astable-frequency
  "Frequency in Hz from resistances (ohms) and capacitance (microfarads)."
  [resistance-1 resistance-2 capacitance]
  (when (or (<= resistance-1 0) (<= resistance-2 0) (<= capacitance 0))
    (throw (IllegalArgumentException. "All values must be positive")))
  (* (/ 1.44 (* (+ resistance-1 (* 2 resistance-2)) capacitance)) 1e6))

(defn astable-duty-cycle
  "Duty cycle in percent from the two resistances (ohms)."
  [resistance-1 resistance-2]
  (when (or (<= resistance-1 0) (<= resistance-2 0))
    (throw (IllegalArgumentException. "All values must be positive")))
  (* (/ (+ resistance-1 resistance-2)
        (+ resistance-1 (* 2 resistance-2)))
     100))
