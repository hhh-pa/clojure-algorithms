(ns clojure-algorithms.electronics.power-factor-correction
  "Shunt capacitance/inductance needed for power-factor correction.
  Ported from TheAlgorithms/Python electronics/power_factor_correction.py.")

(defn- reactive-power-difference
  "Difference between current and expected reactive power (delta Q)."
  [frequency voltage real-power current-pf expected-pf]
  (doseq [pf [current-pf expected-pf]]
    (when (or (not (number? pf)) (< pf -1) (> pf 1))
      (throw (IllegalArgumentException.
              "power_factor must be a valid float value between -1 and 1."))))
  (when (zero? frequency)
    (throw (IllegalArgumentException. "frequency is zero dc circuit")))
  (when (zero? voltage)
    (throw (IllegalArgumentException. "voltage is zero no excitation")))
  (let [q (fn [pf] (* (/ real-power pf) (Math/sin (Math/acos pf))))]
    (- (q current-pf) (q expected-pf))))

(defn shunt-capacitor-power-factor-correction
  "Shunt capacitance in farads to reach `expected-pf`."
  [voltage frequency real-power current-pf expected-pf]
  (/ (reactive-power-difference frequency voltage real-power
                                current-pf expected-pf)
     (* 2 Math/PI frequency voltage voltage)))

(defn shunt-inductor-power-factor-correction
  "Shunt inductance in henries to reach `expected-pf`."
  [voltage frequency real-power current-pf expected-pf]
  (let [delta-q (reactive-power-difference frequency voltage real-power
                                           current-pf expected-pf)]
    (when (zero? delta-q)
      (throw (IllegalArgumentException.
              "current and expected power factors are equal, no correction needed")))
    (/ (* voltage voltage) (* 2 Math/PI frequency delta-q))))
