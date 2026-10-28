(ns clojure-algorithms.bit-manipulation.power-of-4
  "Power-of-four test via bit manipulation: the number must be a power of
  two whose single set bit sits at an odd (1-based) position.")

(defn power-of-4
  "Returns true when positive integer `number` is a power of four."
  [number]
  (when (not (integer? number))
    (throw (ex-info "number must be an integer" {})))
  (when (<= number 0)
    (throw (ex-info "number must be positive" {})))
  (if (zero? (bit-and number (dec number)))
    (odd? (loop [n number, c 0]
            (if (zero? n) c (recur (bit-shift-right n 1) (inc c)))))
    false))
