(ns clojure-algorithms.maths.odd-sieve
  "Odd sieve of Eratosthenes.
https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes")

(defn odd-sieve
  "Returns the prime numbers below num, sieving over odd candidates only."
  [num]
  (if (<= num 2)
    []
    (loop [candidates (vec (range 3 num 2)), primes [2]]
      (if (empty? candidates)
        primes
        (let [p (first candidates)]
          (recur (vec (remove #(zero? (mod % p)) (rest candidates)))
                 (conj primes p)))))))
