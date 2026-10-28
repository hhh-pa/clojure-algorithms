(ns clojure-algorithms.bit-manipulation.count-1s-brian-kernighan-method
  "Count set bits using Brian Kernighan's method (clear the lowest set bit
  each iteration).
  Ref: https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan")

(defn get-1s-count
  "Returns the number of set bits in non-negative integer `number`."
  [number]
  (when (or (not (integer? number)) (neg? number))
    (throw (ex-info "Input must be a non-negative integer" {})))
  (loop [n number, count 0]
    (if (zero? n)
      count
      (recur (bit-and n (dec n)) (inc count)))))
