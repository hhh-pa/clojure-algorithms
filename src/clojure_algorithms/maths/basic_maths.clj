(ns clojure-algorithms.maths.basic-maths
  "Implementation of Basic Math in Python.")

(defn- divide-out
  "Divides `n` by `d` as many times as possible.
  Returns [remaining-n exponent]."
  [n d]
  (loop [n n, e 0]
    (if (zero? (rem n d))
      (recur (quot n d) (inc e))
      [n e])))

(defn- factorize
  "Returns [remaining-n factor-exponent-pairs] after trial division of `n`
  by 2 and then by odd divisors up to sqrt(n)."
  [n]
  (let [[n e2] (divide-out n 2)
        bound (inc (long (Math/sqrt (double n))))]
    (loop [n n, d 3, factors (if (pos? e2) [[2 e2]] [])]
      (if (> d bound)
        [n factors]
        (let [[n e] (divide-out n d)]
          (recur n (+ d 2) (if (pos? e) (conj factors [d e]) factors)))))))

(defn prime-factors
  "Returns the list of prime factors of the positive integer `n`,
  with multiplicity. For example, (prime-factors 100) => [2 2 5 5]."
  [n]
  (when (<= n 0)
    (throw (ex-info "Only positive integers have prime factors" {:type :ValueError})))
  (let [[n factors] (factorize n)
        factors (if (> n 2) (conj factors [n 1]) factors)]
    (vec (mapcat (fn [[p e]] (repeat e p)) factors))))

(defn number-of-divisors
  "Returns the number of divisors of the positive integer `n`."
  [n]
  (when (<= n 0)
    (throw (ex-info "Only positive numbers are accepted" {:type :ValueError})))
  (let [[n factors] (factorize n)
        div (reduce * 1 (map #(inc (second %)) factors))]
    (if (> n 1) (* div 2) div)))

(defn- int-pow
  "Integer power: `base` raised to the non-negative integer `exp`."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn sum-of-divisors
  "Returns the sum of all divisors of the positive integer `n`."
  [n]
  (when (<= n 0)
    (throw (ex-info "Only positive numbers are accepted" {:type :ValueError})))
  (let [[n factors] (factorize n)]
    (reduce * 1 (map (fn [[p e]] (quot (dec (int-pow p (inc e))) (dec p)))
                     factors))))

(defn euler-phi
  "Returns Euler's totient of the positive integer `n`: the count of
  integers in [1, n] that are coprime to `n`."
  [n]
  (when (<= n 0)
    (throw (ex-info "Only positive numbers are accepted" {:type :ValueError})))
  (reduce (fn [s p] (quot (* s (dec p)) p))
          n
          (distinct (prime-factors n))))
