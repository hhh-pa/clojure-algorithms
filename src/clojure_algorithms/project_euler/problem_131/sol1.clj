(ns clojure-algorithms.project-euler.problem-131.sol1
  "Project Euler Problem 131: primes in perfect cubes.

  There are some prime values p for which there exists a positive integer n
  such that n^3 + n^2*p is a perfect cube. For example, when p = 19,
  8^3 + 8^2 * 19 = 12^3. Counts how many primes below one million have this
  remarkable property.")

(defn is-prime
  "Determines whether number is prime."
  [number]
  (every? #(not (zero? (mod number %)))
          (range 2 (inc (long (Math/sqrt number))))))

(defn solution
  "Returns the number of primes below max-prime with the property."
  ([] (solution 1000000))
  ([max-prime]
   (loop [primes-count 0
          cube-index 1
          prime-candidate 7]
     (if (>= prime-candidate max-prime)
       primes-count
       (recur (+ primes-count (if (is-prime prime-candidate) 1 0))
              (inc cube-index)
              (+ prime-candidate (* 6 (inc cube-index))))))))
