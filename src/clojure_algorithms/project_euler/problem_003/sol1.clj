(ns clojure-algorithms.project-euler.problem-003.sol1
  "Project Euler Problem 3: largest prime factor.

  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143?")

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
     (if (prime? n)
       n
       (let [n (loop [n n] (if (even? n) (recur (quot n 2)) n))]
         (if (prime? n)
           n
           (let [limit (inc (long (Math/sqrt (double n))))]
             (loop [i 3 max-number 0]
               (if (>= i limit)
                 max-number
                 (if (zero? (mod n i))
                   (if (prime? (quot n i))
                     (quot n i)
                     (recur (+ i 2) (if (prime? i) i max-number)))
                   (recur (+ i 2) max-number)))))))))))
