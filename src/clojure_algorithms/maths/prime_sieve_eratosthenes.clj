(ns clojure-algorithms.maths.prime-sieve-eratosthenes
  "Sieve of Eratosthenes\n\nInput: n = 10\nOutput: 2 3 5 7\n\nInput: n = 20\nOutput: 2 3 5 7 11 13 17 19\n\nyou can read in detail about this at\nhttps://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare prime-sieve-eratosthenes)

(defn prime-sieve-eratosthenes "Print the prime numbers up to n\n\n>>> prime_sieve_eratosthenes(10)\n[2, 3, 5, 7]\n>>> prime_sieve_eratosthenes(20)\n[2, 3, 5, 7, 11, 13, 17, 19]\n>>> prime_sieve_eratosthenes(2)\n[2]\n>>> prime_sieve_eratosthenes(1)\n[]\n>>> prime_sieve_eratosthenes(-1)\nTraceback (most recent call last):\n...\nValueError: Input must be a positive integer"
  [num] (let [i (atom nil) p (atom nil) primes (atom nil)] (try (when (py-truthy (py-le num 0)) (throw (ex-info (py-str "Input must be a positive integer") {:type :ValueError}))) (reset! primes (py-mul [true] (py-add num 1))) (reset! p 2) (while (py-truthy (py-le (py-mul @p @p) num)) (when (py-truthy (py-get @primes @p)) (doseq [__i1 (py-iter (range (py-mul @p @p) (py-add num 1) @p))] (reset! i __i1) (swap! primes (fn [__c] (py-assoc-in __c [@i] false))))) (swap! p py-add 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [prime (py-iter (range 2 (py-add num 1))) :when (py-truthy (py-get @primes prime))] prime))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
