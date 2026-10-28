(ns clojure-algorithms.project-euler.problem-005.sol1
  "Project Euler Problem 5: smallest multiple.

  2520 is the smallest number that can be divided by each of the numbers
  from 1 to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all
  of the numbers from 1 to 20?

  This solution brute-forces multiples of n * (n - 1).")

(defn- coerce-int
  "Coerces n to an integer, throwing an ex-info mimicking Python's TypeError
  when n is not int-like."
  [n]
  (cond (integer? n) n
        (number? n) (long n)
        (and (string? n) (re-matches #"\s*[+-]?\d+\s*" n))
        (Long/parseLong (clojure.string/trim n))
        :else (throw (ex-info "Parameter n must be int or castable to int."
                              {:value n}))))

(defn solution
  "Returns the smallest positive number that is evenly divisible (divisible
  with no remainder) by all of the numbers from 1 to n."
  ([] (solution 20))
  ([n]
   (let [n (coerce-int n)]
     (when (<= n 0)
       (throw (ex-info "Parameter n must be greater than or equal to one."
                       {:value n})))
     (loop [i 0]
       (let [i (+ i (* n (dec n)))]
         (if (every? #(zero? (mod i %)) (range 2 n))
           (if (zero? i) 1 i)
           (recur i)))))))
