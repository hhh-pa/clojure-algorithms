(ns clojure-algorithms.maths.modular-exponential
  "Modular Exponential.
Modular exponentiation is a type of exponentiation performed over a modulus.
https://en.wikipedia.org/wiki/Modular_exponentiation")

(defn modular-exponential
  "Calculate (base ** power) % mod with binary exponentiation.
Returns -1 for a negative power."
  [base power mod]
  (if (neg? power)
    -1
    (loop [b (clojure.core/mod base mod), p power, result 1]
      (if (zero? p)
        result
        (recur (clojure.core/mod (*' b b) mod)
               (quot p 2)
               (if (odd? p)
                 (clojure.core/mod (*' result b) mod)
                 result))))))

(defn main
  "Call Modular Exponential Function."
  []
  (println (modular-exponential 3 200 13))
  nil)
