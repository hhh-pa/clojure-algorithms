(ns clojure-algorithms.electronics.capacitor-equivalence
  "Equivalent capacitance for capacitors in series or parallel.
  Ported from TheAlgorithms/Python electronics/capacitor_equivalence.py.")

(defn capacitor-parallel
  "Equivalent capacitance: C1 + C2 + ... + Cn."
  [capacitors]
  (reduce (fn [sum [index c]]
            (when (neg? c)
              (throw (IllegalArgumentException.
                      (str "Capacitor at index " index " has a negative value!"))))
            (+ sum c))
          0.0
          (map-indexed vector capacitors)))

(defn capacitor-series
  "Equivalent capacitance: 1 / (1/C1 + 1/C2 + ... + 1/Cn)."
  [capacitors]
  (let [reciprocal (reduce (fn [sum [index c]]
                             (when (<= c 0)
                               (throw (IllegalArgumentException.
                                       (str "Capacitor at index " index
                                            " has a negative or zero value!"))))
                             (+ sum (/ 1.0 c)))
                           0.0
                           (map-indexed vector capacitors))]
    (/ 1.0 reciprocal)))
