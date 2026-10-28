(ns clojure-algorithms.maths.perfect-number
  "== Perfect Number ==
In number theory, a perfect number is a positive integer that is equal to the sum of
its positive divisors, excluding the number itself.

https://en.wikipedia.org/wiki/Perfect_number
https://oeis.org/A000396")

(defn- proper-divisor-sum
  "Sum of proper divisors of n by scanning 1..n//2."
  [n]
  (let [n (long n)
        half (quot n 2)]
    (loop [i 1 s 0]
      (if (> i half)
        s
        (recur (inc i) (if (zero? (rem n i)) (+ s i) s))))))

(defn perfect
  "Check if a number is a perfect number: a positive integer equal to
the sum of its proper divisors."
  [number]
  (when-not (integer? number)
    (throw (ex-info "number must be an integer" {:type :ValueError})))
  (cond
    (<= number 0) false
    (= number 1) false
    :else (= (proper-divisor-sum number) number)))

(defn perfect-optimized
  "Optimized perfect number checker using divisor pairs (d, n/d),
reducing the search space to sqrt(n)."
  [number]
  (when-not (integer? number)
    (throw (ex-info "number must be an integer" {:type :ValueError})))
  (if (<= number 1)
    false
    (let [number (long number)]
      (loop [i 2 divisor-sum 1]
        (if (> (* i i) number)
          (= divisor-sum number)
          (recur (inc i)
                 (if (zero? (rem number i))
                   (let [paired (quot number i)]
                     (+ divisor-sum i (if (= i paired) 0 paired)))
                   divisor-sum)))))))

(defn find-perfect-numbers
  "Find all perfect numbers up to a given limit."
  [limit]
  (when (or (not (integer? limit)) (neg? limit))
    (throw (ex-info "limit must be a non-negative integer" {:type :ValueError})))
  (filterv perfect (range 1 (inc limit))))

(defn get-divisors
  "Get all proper divisors of a number (excluding the number itself),
in ascending order."
  [number]
  (when (or (not (integer? number)) (<= number 0))
    (throw (ex-info "number must be a positive integer" {:type :ValueError})))
  (if (= number 1)
    []
    (filterv #(zero? (rem number %)) (range 1 (inc (quot number 2))))))
