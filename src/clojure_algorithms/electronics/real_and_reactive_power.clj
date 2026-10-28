(ns clojure-algorithms.electronics.real-and-reactive-power
  "Real (P = S * pf) and reactive (Q = S * sqrt(1 - pf^2)) power
  from apparent power and power factor.
  Ported from TheAlgorithms/Python electronics/real_and_reactive_power.py.")

(defn- check-power-factor
  "Throw unless `pf` is a number in [-1, 1]."
  [pf]
  (when (or (not (number? pf)) (< pf -1) (> pf 1))
    (throw (IllegalArgumentException.
            "power_factor must be a valid float value between -1 and 1."))))

(defn real-power
  "Real power from apparent power and power factor."
  [apparent-power power-factor]
  (check-power-factor power-factor)
  (* apparent-power power-factor))

(defn reactive-power
  "Reactive power from apparent power and power factor."
  [apparent-power power-factor]
  (check-power-factor power-factor)
  (* apparent-power (Math/sqrt (- 1 (* power-factor power-factor)))))
