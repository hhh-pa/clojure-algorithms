(ns clojure-algorithms.electronics.electric-conductivity
  "Conductivity, electron concentration or mobility from the other two.

  Exactly one argument must be zero (the unknown); returns [name value].
  Ported from TheAlgorithms/Python electronics/electric_conductivity.py.")

(def ^:const electron-charge
  "Elementary charge in coulombs."
  1.6021e-19)

(defn electric-conductivity
  "Solve for the zero-valued quantity among conductivity, electron-conc, mobility."
  [conductivity electron-conc mobility]
  (when (not= 1 (count (filter zero? [conductivity electron-conc mobility])))
    (throw (IllegalArgumentException.
            "You cannot supply more or less than 2 values")))
  (cond
    (neg? conductivity)
    (throw (IllegalArgumentException. "Conductivity cannot be negative"))
    (neg? electron-conc)
    (throw (IllegalArgumentException.
            "Electron concentration cannot be negative"))
    (neg? mobility)
    (throw (IllegalArgumentException. "mobility cannot be negative"))
    (zero? conductivity)
    ["conductivity" (* mobility electron-conc electron-charge)]
    (zero? electron-conc)
    ["electron_conc" (/ conductivity (* mobility electron-charge))]
    :else
    ["mobility" (/ conductivity (* electron-conc electron-charge))]))
