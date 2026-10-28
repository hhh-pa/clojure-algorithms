(ns clojure-algorithms.electronics.resistor-equivalence
  "Equivalent resistance for resistors in series or parallel.
  Ported from TheAlgorithms/Python electronics/resistor_equivalence.py.")

(defn resistor-parallel
  "Equivalent resistance: 1 / (1/R1 + 1/R2 + ... + 1/Rn)."
  [resistors]
  (let [reciprocal (reduce (fn [sum [index r]]
                             (when (<= r 0)
                               (throw (IllegalArgumentException.
                                       (str "Resistor at index " index
                                            " has a negative or zero value!"))))
                             (+ sum (/ 1.0 r)))
                           0.0
                           (map-indexed vector resistors))]
    (/ 1.0 reciprocal)))

(defn resistor-series
  "Equivalent resistance: R1 + R2 + ... + Rn."
  [resistors]
  (reduce (fn [sum [index r]]
            (when (neg? r)
              (throw (IllegalArgumentException.
                      (str "Resistor at index " index " has a negative value!"))))
            (+ sum r))
          0.0
          (map-indexed vector resistors)))
