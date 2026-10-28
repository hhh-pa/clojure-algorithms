(ns clojure-algorithms.project-euler.problem-010.sol3
  "Project Euler Problem 10: summation of primes.

  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.

  This solution uses the sieve of Eratosthenes, one of the most efficient
  ways to find all primes smaller than n when n is smaller than 10
  million. Only for positive numbers.")

(defn solution
  "Returns the sum of all the primes below n using the sieve of
  Eratosthenes."
  ([] (solution 2000000))
  ([n]
   (let [sieve (boolean-array (inc n) true)]
     (aset sieve 0 false)
     (aset sieve 1 false)
     (doseq [i (range 2 (inc (long (Math/sqrt (double n)))))
             :when (aget sieve i)]
       (doseq [j (range (* i i) (inc n) i)]
         (aset sieve j false)))
     (reduce + 0 (for [i (range n) :when (aget sieve i)] i)))))
