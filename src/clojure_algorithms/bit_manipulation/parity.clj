(ns clojure-algorithms.bit-manipulation.parity
  "Parity of an integer: 1 when the number of set bits is odd, else 0.")

(defn parity
  "Returns 1 when non-negative integer `number` has an odd number of set
  bits, otherwise 0. Uses Kernighan's bit-clearing loop."
  [number]
  (when (neg? number)
    (throw (ex-info "number must not be negative" {})))
  (loop [n number, p 0]
    (if (zero? n)
      p
      (recur (bit-and n (dec n)) (bit-xor p 1)))))
