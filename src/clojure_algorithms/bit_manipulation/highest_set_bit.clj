(ns clojure-algorithms.bit-manipulation.highest-set-bit
  "Position of the highest set bit of an integer (1-based; 0 has none).
  Ref: https://graphics.stanford.edu/~seander/bithacks.html#IntegerLogObvious")

(defn get-highest-set-bit-position
  "Returns the 1-based position of the highest set bit of `number`,
  or 0 when `number` is 0. `number` must be an integer."
  [number]
  (when (not (integer? number))
    (throw (ex-info "Input value must be an 'int' type" {})))
  (loop [n number, position 0]
    (if (zero? n)
      position
      (recur (bit-shift-right n 1) (inc position)))))
