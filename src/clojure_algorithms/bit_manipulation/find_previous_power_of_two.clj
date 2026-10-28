(ns clojure-algorithms.bit-manipulation.find-previous-power-of-two
  "Largest power of two less than or equal to a given integer.
  See https://stackoverflow.com/questions/1322510")

(defn find-previous-power-of-two
  "Returns the largest power of two <= `number` (0 maps to 0).
  `number` must be a non-negative integer."
  [number]
  (when (or (not (integer? number)) (neg? number))
    (throw (ex-info "Input must be a non-negative integer" {})))
  (if (zero? number)
    0
    (loop [power 1N]
      (if (<= power number)
        (recur (* 2 power))
        (let [result (if (> number 1) (quot power 2) 1)]
          (if (<= result Long/MAX_VALUE) (long result) result))))))
