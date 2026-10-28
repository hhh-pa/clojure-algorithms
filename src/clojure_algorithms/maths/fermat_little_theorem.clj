(ns clojure-algorithms.maths.fermat-little-theorem
  "Fermat's little theorem: (a / b) mod p always equals
a * (b ^ (p - 2)) mod p, where p is prime, b divides a,
and p doesn't divide b.
Wikipedia reference: https://en.wikipedia.org/wiki/Fermat%27s_little_theorem")

(defn binary-exponentiation
  "Calculate (a ** n) % mod using binary exponentiation, O(log n) time."
  [a n mod-v]
  (cond
    (zero? n) 1
    (odd? n) (mod (*' (binary-exponentiation a (dec n) mod-v) a) mod-v)
    :else (let [b (binary-exponentiation a (quot n 2) mod-v)]
            (mod (*' b b) mod-v))))

(def p
  "A prime number used in the Fermat's little theorem demo."
  701)

(def a
  "Dividend used in the Fermat's little theorem demo."
  1000000000)

(def b
  "Divisor used in the Fermat's little theorem demo."
  10)
