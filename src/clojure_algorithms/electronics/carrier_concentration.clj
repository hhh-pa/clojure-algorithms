(ns clojure-algorithms.electronics.carrier-concentration
  "Electron, hole or intrinsic carrier concentration from the other two.

  Exactly one argument must be zero (the unknown); returns [name value].
  Ported from TheAlgorithms/Python electronics/carrier_concentration.py.")

(defn carrier-concentration
  "Solve for the zero-valued concentration among electron, hole, intrinsic."
  [electron-conc hole-conc intrinsic-conc]
  (when (not= 1 (count (filter zero? [electron-conc hole-conc intrinsic-conc])))
    (throw (IllegalArgumentException.
            "You cannot supply more or less than 2 values")))
  (cond
    (neg? electron-conc)
    (throw (IllegalArgumentException.
            "Electron concentration cannot be negative in a semiconductor"))
    (neg? hole-conc)
    (throw (IllegalArgumentException.
            "Hole concentration cannot be negative in a semiconductor"))
    (neg? intrinsic-conc)
    (throw (IllegalArgumentException.
            "Intrinsic concentration cannot be negative in a semiconductor"))
    (zero? electron-conc)
    ["electron_conc" (/ (* intrinsic-conc intrinsic-conc) (double hole-conc))]
    (zero? hole-conc)
    ["hole_conc" (/ (* intrinsic-conc intrinsic-conc) (double electron-conc))]
    :else
    ["intrinsic_conc" (Math/sqrt (* electron-conc hole-conc))]))
