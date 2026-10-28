(ns clojure-algorithms.maths.primelib
  "Created on Thu Oct  5 16:44:23 2017

  @author: Christian Bender

  This library contains some useful functions to deal with
  prime numbers and whole numbers.

  Overview:

  is-prime(number)
  sieve-er(N)
  get-prime-numbers(N)
  prime-factorization(number)
  greatest-prime-factor(number)
  smallest-prime-factor(number)
  get-prime(n)
  get-primes-between(pNumber1, pNumber2)

  ----

  is-even(number)
  is-odd(number)
  kg-v(number1, number2)  // least common multiple
  get-divisors(number)    // all divisors of 'number' inclusive 1, number
  is-perfect-number(number)

  NEW-FUNCTIONS

  simplify-fraction(numerator, denominator)
  factorial (n) // n!
  fib (n) // calculate the n-th fibonacci term.

  -----

  goldbach(number)  // Goldbach's assumption")

(defn is-prime
  "Returns true if 'number' is prime, otherwise false."
  [number]
  (assert (and (integer? number) (>= number 0))
          "'number' must been an int and positive")
  (and (> number 1)
       (not-any? #(zero? (mod number %))
                 (range 2 (inc (Math/round (Math/sqrt (double number))))))))

(defn sieve-er
  "Returns a list of prime numbers from 2 up to N.
  This function implements the algorithm called sieve of erathostenes."
  [n]
  (assert (and (integer? n) (> n 2)) "'N' must been an int and > 2")
  (let [nums (vec (range 2 (inc n)))]
    (loop [i 0 sieved nums]
      (if (>= i (count sieved))
        (filterv pos? sieved)
        (let [p (sieved i)]
          (recur (inc i)
                 (if (zero? p)
                   sieved
                   (vec (map-indexed
                         (fn [j x]
                           (if (and (> j i) (zero? (mod x p))) 0 x))
                         sieved)))))))))

(defn get-prime-numbers
  "Returns a list of prime numbers from 2 up to N (inclusive).
  This function is more efficient than function 'sieve-er'."
  [n]
  (assert (and (integer? n) (> n 2)) "'N' must been an int and > 2")
  (filterv is-prime (range 2 (inc n))))

(defn prime-factorization
  "Returns a list of the prime number factors of 'number'."
  [number]
  (assert (and (integer? number) (>= number 0))
          "'number' must been an int and >= 0")
  (cond (<= number 1) [number]
        (is-prime number) [number]
        :else (loop [quotient number factor 2 acc []]
                (if (= quotient 1)
                  acc
                  (if (and (is-prime factor) (zero? (mod quotient factor)))
                    (recur (quot quotient factor) factor (conj acc factor))
                    (recur quotient (inc factor) acc))))))

(defn greatest-prime-factor
  "Returns the greatest prime number factor of 'number'."
  [number]
  (assert (and (integer? number) (>= number 0))
          "'number' must been an int and >= 0")
  (reduce max (prime-factorization number)))

(defn smallest-prime-factor
  "Returns the smallest prime number factor of 'number'."
  [number]
  (assert (and (integer? number) (>= number 0))
          "'number' must been an int and >= 0")
  (reduce min (prime-factorization number)))

(defn is-even
  "Returns true if 'number' is even, otherwise false."
  [number]
  (assert (integer? number) "'number' must been an int")
  (even? number))

(defn is-odd
  "Returns true if 'number' is odd, otherwise false."
  [number]
  (assert (integer? number) "'number' must been an int")
  (odd? number))

(defn goldbach
  "Goldbach's assumption: returns a list of two prime numbers whose
  sum is equal to 'number' (an even positive integer > 2)."
  [number]
  (assert (and (integer? number) (> number 2) (is-even number))
          "'number' must been an int, even and > 2")
  (let [primes (get-prime-numbers number)
        len-pn (count primes)
        ans (first (for [i (range len-pn)
                         j (range (inc i) len-pn)
                         :when (= number (+ (primes i) (primes j)))]
                     [(primes i) (primes j)]))]
    (assert (and (vector? ans)
                 (= 2 (count ans))
                 (= number (reduce + ans))
                 (is-prime (ans 0))
                 (is-prime (ans 1)))
            "'ans' must contains two primes. And sum of elements must been eq 'number'")
    ans))

(defn- gcd
  "Greatest common divisor via Euclid's algorithm."
  [a b]
  (loop [a a b b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn kg-v
  "Least common multiple of positive integers 'number1' and 'number2'."
  [number1 number2]
  (assert (and (integer? number1) (integer? number2)
               (>= number1 1) (>= number2 1))
          "'number1' and 'number2' must been positive integer.")
  (quot (* number1 number2) (gcd number1 number2)))

(defn get-prime
  "Gets the n-th prime number (0-indexed)."
  [n]
  (assert (and (integer? n) (>= n 0)) "'number' must been a positive int")
  (nth (filter is-prime (iterate inc 2)) n))

(defn get-primes-between
  "Returns a list of all prime numbers between 'p-number-1' (exclusive)
  and 'p-number-2' (exclusive)."
  [p-number-1 p-number-2]
  (assert (and (is-prime p-number-1) (is-prime p-number-2)
               (< p-number-1 p-number-2))
          "The arguments must been prime numbers and 'pNumber1' < 'pNumber2'")
  (filterv is-prime (range (inc p-number-1) p-number-2)))

(defn get-divisors
  "Returns all divisors of n (inclusive 1 and 'n')."
  [n]
  (assert (and (integer? n) (>= n 1)) "'n' must been int and >= 1")
  (filterv #(zero? (mod n %)) (range 1 (inc n))))

(defn is-perfect-number
  "Returns true if 'number' is a perfect number, otherwise false."
  [number]
  (assert (and (integer? number) (> number 1))
          "'number' must been an int and >= 1")
  (= number (reduce + (butlast (get-divisors number)))))

(defn simplify-fraction
  "Returns a vector with the simplified numerator and denominator."
  [numerator denominator]
  (assert (and (integer? numerator) (integer? denominator)
               (not= denominator 0))
          "The arguments must been from type int and 'denominator' != 0")
  (let [g (gcd (abs (long numerator)) (abs (long denominator)))]
    [(quot numerator g) (quot denominator g)]))

(defn factorial
  "Returns the factorial of 'n' (n!)."
  [n]
  (assert (and (integer? n) (>= n 0)) "'n' must been a int and >= 0")
  (reduce *' 1 (range 1 (inc n))))

(defn fib
  "Returns the n'th fibonacci term, indexing by 0
  (i.e. fib(0) = 1, fib(1) = 1, fib(5) = 8)."
  [n]
  (assert (and (integer? n) (>= n 0)) "'n' must been an int and >= 0")
  (loop [i 0 ans 1 fib1 1]
    (if (>= i (dec n))
      ans
      (recur (inc i) (+' ans fib1) ans))))
