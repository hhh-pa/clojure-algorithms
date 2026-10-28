(ns clojure-algorithms.bit-manipulation.count-number-of-one-bits
  "Three ways to count set bits in a non-negative integer: Brian Kernighan's
  algorithm, the modulo operator, and a precomputed lookup table.")

(defn get-set-bits-count-using-brian-kernighans-algorithm
  "Counts set bits by repeatedly clearing the lowest set bit."
  [number]
  (when (neg? number)
    (throw (ex-info "the value of input must not be negative" {})))
  (loop [n number, result 0]
    (if (zero? n)
      result
      (recur (bit-and n (dec n)) (inc result)))))

(defn get-set-bits-count-using-modulo-operator
  "Counts set bits by shifting right and testing each bit with mod 2."
  [number]
  (when (neg? number)
    (throw (ex-info "the value of input must not be negative" {})))
  (loop [n number, result 0]
    (if (zero? n)
      result
      (recur (bit-shift-right n 1) (if (= 1 (mod n 2)) (inc result) result)))))

(def ^:private lookup-table
  "Precomputed popcount for every byte value."
  (vec (map #(Long/bitCount (long %)) (range 256))))

(defn get-set-bits-count-using-lookup-table
  "Counts set bits by splitting into four bytes and summing table lookups."
  [number]
  (when (neg? number)
    (throw (ex-info "the value of input must not be negative" {})))
  (+ (lookup-table (bit-and number 0xFF))
     (lookup-table (bit-and (bit-shift-right number 8) 0xFF))
     (lookup-table (bit-and (bit-shift-right number 16) 0xFF))
     (lookup-table (bit-and (bit-shift-right number 24) 0xFF))))
