(ns clojure-algorithms.project-euler.problem-007.sol2
  "Project Euler Problem 7: 10001st prime.

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
  can see that the 6th prime is 13.

  What is the 10001st prime number?

  This solution collects primes until it has seen n of them, and
  validates its argument.")

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

(defn- coerce-int
  "Coerces n to an integer, throwing an ex-info mimicking Python's TypeError
  when n is not int-like."
  [n]
  (cond (integer? n) n
        (number? n) (long n)
        (and (string? n) (re-matches #"\s*[+-]?\d+\s*" n))
        (Long/parseLong (clojure.string/trim n))
        :else (throw (ex-info "Parameter nth must be int or castable to int."
                              {:value n}))))

(defn solution
  "Returns the n-th prime number."
  ([] (solution 10001))
  ([n]
   (let [n (coerce-int n)]
     (when (<= n 0)
       (throw (ex-info "Parameter nth must be greater than or equal to one."
                       {:value n})))
     (loop [primes [] num 2]
       (if (>= (count primes) n)
         (peek primes)
         (recur (if (prime? num) (conj primes num) primes) (inc num)))))))
