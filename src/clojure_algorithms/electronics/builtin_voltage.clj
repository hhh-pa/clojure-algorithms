(ns clojure-algorithms.electronics.builtin-voltage
  "Built-in voltage of a pn junction diode.

  Physical constants (Boltzmann constant, electron volt) are inlined
  here instead of depending on scipy.constants.
  Ported from TheAlgorithms/Python electronics/builtin_voltage.py.")

(def ^:const temperature
  "Temperature in kelvin."
  300)

(def ^:const boltzmann
  "Boltzmann constant in J/K."
  1.380649e-23)

(def ^:const electron-volt
  "One electron volt in joules."
  1.602176634e-19)

(defn builtin-voltage
  "Built-in voltage from donor, acceptor and intrinsic concentrations."
  [donor-conc acceptor-conc intrinsic-conc]
  (cond
    (<= donor-conc 0)
    (throw (IllegalArgumentException. "Donor concentration should be positive"))
    (<= acceptor-conc 0)
    (throw (IllegalArgumentException. "Acceptor concentration should be positive"))
    (<= intrinsic-conc 0)
    (throw (IllegalArgumentException. "Intrinsic concentration should be positive"))
    (<= donor-conc intrinsic-conc)
    (throw (IllegalArgumentException.
            "Donor concentration should be greater than intrinsic concentration"))
    (<= acceptor-conc intrinsic-conc)
    (throw (IllegalArgumentException.
            "Acceptor concentration should be greater than intrinsic concentration"))
    :else (/ (* boltzmann temperature
                (Math/log (/ (* donor-conc acceptor-conc)
                             (* intrinsic-conc intrinsic-conc))))
             electron-volt)))
