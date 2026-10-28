(ns clojure-algorithms.electronics.ohms-law
  "Ohm's law: solve for the zero-valued quantity among voltage,
  current and resistance. Returns a {name value} map.
  Ported from TheAlgorithms/Python electronics/ohms_law.py.")

(defn ohms-law
  "Apply V = I * R; exactly one argument must be zero."
  [voltage current resistance]
  (when (not= 1 (count (filter zero? [voltage current resistance])))
    (throw (IllegalArgumentException. "One and only one argument must be 0")))
  (when (neg? resistance)
    (throw (IllegalArgumentException. "Resistance cannot be negative")))
  (cond
    (zero? voltage) {"voltage" (double (* current resistance))}
    (zero? current) {"current" (double (/ voltage resistance))}
    :else {"resistance" (double (/ voltage current))}))
