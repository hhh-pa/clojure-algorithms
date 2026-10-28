(ns clojure-algorithms.bit-manipulation.largest-pow-of-two-le-num
  "Largest power of two less than or equal to a given number, found by
  left-shifting a set bit while (res << 1) <= number.")

(defn largest-pow-of-two-le-num
  "Returns the largest power of two <= `number`, or 0 for non-positive
  input. Floats raise."
  [number]
  (when (float? number)
    (throw (ex-info "Input value must be a 'int' type" {})))
  (if (<= number 0)
    0
    (loop [res 1N]
      (if (<= (* 2 res) number)
        (recur (* 2 res))
        (if (<= res Long/MAX_VALUE) (long res) res)))))
