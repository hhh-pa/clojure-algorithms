(ns clojure-algorithms.maths.twin-prime
  "== Twin Prime ==
A number n+2 is said to be a Twin prime of number n if
both n and n+2 are prime.

Examples of Twin pairs: (3, 5), (5, 7), (11, 13), (17, 19), (29, 31), (41, 43), ...
https://en.wikipedia.org/wiki/Twin_prime")

(defn- prime?
  "Primality check mirroring maths/prime_check.py: only non-negative
integers are accepted, otherwise :ValueError."
  [n]
  (when (or (not (integer? n)) (neg? n))
    (throw (ex-info "is_prime() only accepts positive integers"
                    {:type :ValueError})))
  (cond
    (< 1 n 4) true
    (or (< n 2) (zero? (mod n 2)) (zero? (mod n 3))) false
    :else (let [limit (Math/sqrt n)]
            (loop [i 5]
              (cond
                (> i limit) true
                (or (zero? (mod n i)) (zero? (mod n (+ i 2)))) false
                :else (recur (+ i 6)))))))

(defn twin-prime
  "Returns n+2 if n and n+2 are prime numbers, and -1 otherwise."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (if (and (prime? number) (prime? (+ number 2)))
    (+ number 2)
    -1))
