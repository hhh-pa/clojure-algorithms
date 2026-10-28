(ns clojure-algorithms.project-euler.problem-007.sol3
  "Project Euler Problem 7: 10001st prime.

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
  can see that the 6th prime is 13.

  What is the 10001st prime number?

  This solution draws the n-th prime from a lazy prime sequence.")

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
  "Returns the n-th prime number."
  ([] (solution 10001))
  ([n]
   (nth (prime-generator) (dec n))))
