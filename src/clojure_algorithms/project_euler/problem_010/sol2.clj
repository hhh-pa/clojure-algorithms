(ns clojure-algorithms.project-euler.problem-010.sol2
  "Project Euler Problem 10: summation of primes.

  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.

  This solution sums primes drawn from a lazy prime sequence.")

(defn prime?
  "Checks to see if a number is prime in O(sqrt(n)).
  A number is prime if it has exactly two factors: 1 and itself.
  Returns a boolean representing primality of the given number."
  [number]
  (cond (and (< 1 number) (< number 4)) true
        (or (< number 2)
            (zero? (mod number 2))
            (zero? (mod number 3))) false
        :else (let [limit (inc (long (Math/sqrt (double number))))]
                (loop [i 5]
                  (cond (>= i limit) true
                        (or (zero? (mod number i))
                            (zero? (mod number (+ i 2)))) false
                        :else (recur (+ i 6)))))))

(defn prime-generator
  "Returns a lazy sequence of prime numbers."
  []
  (filter prime? (iterate inc 2)))

(defn solution
  "Returns the sum of all the primes below n."
  ([] (solution 2000000))
  ([n]
   (reduce + 0 (take-while #(< % n) (prime-generator)))))
