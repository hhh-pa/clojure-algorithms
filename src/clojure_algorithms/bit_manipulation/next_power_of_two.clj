(ns clojure-algorithms.bit-manipulation.next-power-of-two
  "Smallest power of two greater than or equal to n, for positive integers.")

(defn next-power-of-two
  "Returns the smallest power of two >= `n`. `n` must be positive."
  [n]
  (when (<= n 0)
    (throw (ex-info "n must be positive" {})))
  ;; BigInteger interop throughout: Clojure's bit-* fns reject BigInt
  ;; operands and Java masks long shift distances mod 64.
  (let [one (java.math.BigInteger/valueOf 1)
        nbi (biginteger n)]
    (if (.equals (.and nbi (.subtract nbi one)) java.math.BigInteger/ZERO)
      n
      (let [w (reduce (fn [acc s] (.or ^java.math.BigInteger acc
                                       (.shiftRight ^java.math.BigInteger acc s)))
                      (.subtract nbi one)
                      [1 2 4 8 16])]
        (loop [w w, shift 32]
          (if (<= 0 (.compareTo ^java.math.BigInteger w (.shiftLeft one shift)))
            (recur (.or w (.shiftRight w shift)) (* 2 shift))
            (let [result (.add w one)]
              (if (<= (.compareTo result
                                  (java.math.BigInteger/valueOf Long/MAX_VALUE))
                      0)
                (.longValue result)
                (bigint result)))))))))
