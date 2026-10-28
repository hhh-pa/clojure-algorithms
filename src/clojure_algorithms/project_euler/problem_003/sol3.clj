(ns clojure-algorithms.project-euler.problem-003.sol3
  "Project Euler Problem 3: largest prime factor.

  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143?

  This solution repeatedly finds the smallest divisor, divides it out, and
  remembers the last divisor found.")

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
     (if (= n 2)
       2
       (loop [n n i 2 ans 0]
         (if (<= n 2)
           (long ans)
           (let [i (loop [i i] (if (zero? (mod n i)) i (recur (inc i))))
                 n (loop [n n] (if (zero? (mod n i)) (recur (quot n i)) n))]
             (recur n (inc i) i))))))))
