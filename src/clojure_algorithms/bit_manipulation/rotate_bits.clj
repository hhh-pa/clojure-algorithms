(ns clojure-algorithms.bit-manipulation.rotate-bits
  "Bit rotation helpers for 32-bit unsigned integers.")

(def ^:private mask32 (dec (bit-shift-left 1 32)))

(defn rotate-left32
  "Rotates the lower 32 bits of non-negative integer `x` left by `k`
  (non-negative) and returns a value in 0..2^32-1."
  [x k]
  (when (or (not (integer? x)) (neg? x))
    (throw (ex-info "x must be a non-negative integer" {})))
  (when (or (not (integer? k)) (neg? k))
    (throw (ex-info "k must be non-negative" {})))
  (let [k (bit-and k 31)]
    (if (zero? k)
      (bit-and x mask32)
      (bit-or (bit-and (bit-shift-left x k) mask32)
              (bit-shift-right (bit-and x mask32) (- 32 k))))))

(defn rotate-right32
  "Rotates the lower 32 bits of non-negative integer `x` right by `k`
  (non-negative) and returns a value in 0..2^32-1."
  [x k]
  (when (or (not (integer? x)) (neg? x))
    (throw (ex-info "x must be a non-negative integer" {})))
  (when (or (not (integer? k)) (neg? k))
    (throw (ex-info "k must be non-negative" {})))
  (let [k (bit-and k 31)]
    (if (zero? k)
      (bit-and x mask32)
      (bit-or (bit-shift-right (bit-and x mask32) k)
              (bit-and (bit-shift-left x (- 32 k)) mask32)))))
