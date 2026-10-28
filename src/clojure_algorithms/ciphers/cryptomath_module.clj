(ns clojure-algorithms.ciphers.cryptomath-module
  "Modular arithmetic helpers for the ciphers.")

(defn- gcd
  [a b]
  (loop [a (bigint a) b (bigint b)]
    (if (zero? b)
      (if (neg? a) (- a) a)
      (recur b (mod a b)))))

(defn find-mod-inverse
  "Returns the modular inverse of a mod m via extended Euclid."
  [a m]
  (when (not= 1 (gcd a m))
    (throw (IllegalArgumentException.
            (format "mod inverse of %s and %s does not exist" a m))))
  (loop [u1 1 u2 0 u3 a v1 0 v2 1 v3 m]
    (if (zero? v3)
      (mod u1 m)
      (let [q (quot u3 v3)]
        (recur v1 v2 v3 (- u1 (* q v1)) (- u2 (* q v2)) (- u3 (* q v3)))))))
