(ns clojure-algorithms.project-euler.problem-003.sol2
  "Project Euler Problem 3: largest prime factor.

  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143?

  This solution divides out factors in increasing order; whatever remains
  above 1 at the end is the largest prime factor.")

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
  "Returns the largest prime factor of the given number n."
  ([] (solution 600851475143))
  ([n]
   (let [n (coerce-int n)]
     (when (<= n 0)
       (throw (ex-info "Parameter n must be greater than or equal to one."
                       {:value n})))
     (loop [n n prime 1 i 2]
       (if (> (* i i) n)
         (long (if (> n 1) n prime))
         (if (zero? (mod n i))
           (recur (quot n i) i i)
           (recur n prime (inc i))))))))
