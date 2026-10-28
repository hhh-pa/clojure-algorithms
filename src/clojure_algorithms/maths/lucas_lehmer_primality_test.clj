(ns clojure-algorithms.maths.lucas-lehmer-primality-test
  "In mathematics, the Lucas-Lehmer test (LLT) is a primality test for Mersenne
  numbers.  https://en.wikipedia.org/wiki/Lucas%E2%80%93Lehmer_primality_test

  A Mersenne number is a number that is one less than a power of two.
  That is M_p = 2^p - 1
  https://en.wikipedia.org/wiki/Mersenne_prime")

(defn lucas-lehmer-test
  "Returns true if the Mersenne number M_p = 2^p - 1 is prime, using the
  Lucas-Lehmer test."
  [p]
  (when (< p 2)
    (throw (ex-info "p should not be less than 2!" {:type :ValueError})))
  (if (== p 2)
    true
    (let [m (dec (reduce *' 1 (repeat (long p) 2)))]
      (zero? (nth (iterate #(mod (- (*' % %) 2) m) 4) (- p 2))))))
