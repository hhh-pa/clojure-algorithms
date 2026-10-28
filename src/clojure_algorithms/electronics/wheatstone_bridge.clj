(ns clojure-algorithms.electronics.wheatstone-bridge
  "Unknown resistance Rx = (R2/R1) * R3 in a balanced Wheatstone bridge.
  Ported from TheAlgorithms/Python electronics/wheatstone_bridge.py.")

(defn wheatstone-solver
  "Unknown resistance from known bridge resistances r1, r2, r3."
  [resistance-1 resistance-2 resistance-3]
  (when (or (<= resistance-1 0) (<= resistance-2 0) (<= resistance-3 0))
    (throw (IllegalArgumentException. "All resistance values must be positive")))
  (double (* (/ resistance-2 resistance-1) resistance-3)))
