(ns clojure-algorithms.ciphers.rsa-factorization
  "RSA modulus factorization from private/public exponents.")

(defn- gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn rsafactor
  "Factors n given exponents d and e; returns sorted [p q]."
  [d e n]
  (let [k (dec (* (bigint d) (bigint e)))]
    (loop []
      (let [g (+ 2 (bigint (rand-int (- n 3))))
            result (loop [t k]
                     (if (odd? t)
                       nil
                       (let [t (quot t 2)
                             x (.modPow (biginteger g) (biginteger t) (biginteger n))
                             y (gcd (dec x) n)]
                         (if (and (> x 1) (> y 1))
                           (sort [(long y) (long (quot n y))])
                           (recur t)))))]
        (if result (vec result) (recur))))))
