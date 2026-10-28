(ns clojure-algorithms.dynamic-programming.fast-fibonacci
  "nth Fibonacci number in O(log n) via the doubling identities:
  F(2n) = F(n)[2F(n+1) - F(n)], F(2n+1) = F(n+1)^2 + F(n)^2.
  Ported from TheAlgorithms/Python dynamic_programming/fast_fibonacci.py.")

(defn- fib-pair
  "Return [F(n) F(n+1)]."
  [n]
  (if (zero? n)
    [0 1]
    (let [[a b] (fib-pair (quot n 2))
          c (* a (- (* 2 b) a))
          d (+ (* a a) (* b b))]
      (if (odd? n) [d (+ c d)] [c d]))))

(defn fibonacci
  "F(n) for non-negative integer `n`."
  [n]
  (when (neg? n)
    (throw (IllegalArgumentException. "Negative arguments are not supported")))
  (first (fib-pair n)))
