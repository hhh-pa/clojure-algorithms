(ns clojure-algorithms.maths.special-numbers.carmichael-number
  "== Carmichael Numbers ==
A number n is said to be a Carmichael number if it
satisfies the following modular arithmetic condition:

    power(b, n-1) MOD n = 1,
    for all b ranging from 1 to n such that b and
    n are relatively prime, i.e, gcd(b, n) = 1

Examples of Carmichael Numbers: 561, 1105, ...
https://en.wikipedia.org/wiki/Carmichael_number")

(defn- gcd
  "Greatest common divisor of two integers via Euclid's algorithm."
  [a b]
  (if (zero? b) a (recur b (rem a b))))

(defn power
  "Modular exponentiation: returns (x**y) mod `mod-v`."
  [x y mod-v]
  (if (zero? y)
    1
    (let [temp (power x (quot y 2) mod-v)
          temp (rem (*' temp temp) mod-v)]
      (if (odd? y)
        (rem (*' temp x) mod-v)
        temp))))

(defn is-carmichael-number
  "Returns true if n is a Carmichael number. Throws an ex-info with :type
:ValueError when n is not a positive integer."
  [n]
  (when (or (not (integer? n)) (<= n 0))
    (throw (ex-info (str "Number " n " must instead be a positive integer")
                    {:type :ValueError})))
  (every? #(= 1 (power % (dec n) n))
          (filter #(= 1 (gcd % n)) (range 2 n))))
