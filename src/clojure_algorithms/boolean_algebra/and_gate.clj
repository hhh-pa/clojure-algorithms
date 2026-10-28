(ns clojure-algorithms.boolean-algebra.and-gate
  "AND gate: 1 only when all inputs are 1.")

(defn- check-bit
  [x list?]
  (when-not (integer? x)
    (throw (IllegalArgumentException.
            (if list?
              "All inputs in the list must be integers"
              "Both inputs must be integers"))))
  (when-not (contains? #{0 1} x)
    (throw (IllegalArgumentException.
            (if list?
              "All inputs in the list must be 0 or 1"
              "Both inputs must be 0 or 1")))))

(defn and-gate
  "Returns the AND of two binary inputs (0 or 1)."
  [input-1 input-2]
  (check-bit input-1 false)
  (check-bit input-2 false)
  (bit-and input-1 input-2))

(defn n-input-and-gate
  "Returns the AND of a non-empty sequential of binary inputs."
  [inputs]
  (when-not (sequential? inputs)
    (throw (IllegalArgumentException. "Input must be a list")))
  (when (empty? inputs)
    (throw (IllegalArgumentException. "Input list cannot be empty")))
  (doseq [item inputs] (check-bit item true))
  (if (every? #(= 1 %) inputs) 1 0))
