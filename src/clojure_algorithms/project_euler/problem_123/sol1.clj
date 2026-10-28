(ns clojure-algorithms.project-euler.problem-123.sol1
  "Project Euler Problem 123: prime square remainders.

  Let pn be the nth prime and r the remainder when (pn-1)^n + (pn+1)^n is
  divided by pn^2. Expanding shows r = 2*pn*n when n is odd and r = 2 when n
  is even. Finds the least value of n for which the remainder first exceeds
  10^10.")

(defn- sieve-from
  "Lazy prime sieve carrying the factor map and next candidate prime."
  [factor-map prime]
  (lazy-seq
   (let [factor (get factor-map prime)
         remaining (dissoc factor-map prime)]
     (if factor
       (let [x (loop [x (+ factor prime)]
                 (if (contains? remaining x)
                   (recur (+ x factor))
                   x))]
         (sieve-from (assoc remaining x factor) (inc prime)))
       (cons prime
             (sieve-from (assoc remaining (* prime prime) prime)
                         (inc prime)))))))

(defn sieve
  "Returns a lazy sequence of prime numbers using the sieve method."
  []
  (sieve-from {} 2))

(defn solution
  "Returns the least value of n for which the remainder first exceeds
  limit."
  ([] (solution 1e10))
  ([limit]
   (loop [primes (sieve)
          n 1]
     (let [prime (first primes)]
       (if (> (* 2 prime n) limit)
         n
         (recur (drop 2 primes) (+ n 2)))))))
