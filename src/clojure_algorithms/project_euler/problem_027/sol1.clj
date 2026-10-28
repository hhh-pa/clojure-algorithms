(ns clojure-algorithms.project-euler.problem-027.sol1
  "Euler Problem 27: find the product of coefficients a and b for the quadratic
  n^2 + a*n + b (|a| < 1000, |b| < 1000) producing the most consecutive primes
  starting at n = 0.")

(defn prime?
  "Checks whether number is prime in O(sqrt(n)). A number is prime if it has
  exactly two factors: 1 and itself."
  [number]
  (cond
    (and (> number 1) (< number 4)) true
    (or (< number 2) (zero? (mod number 2)) (zero? (mod number 3))) false
    :else (let [limit (inc (int (Math/sqrt number)))]
            (loop [i 5]
              (cond
                (>= i limit) true
                (or (zero? (mod number i)) (zero? (mod number (+ i 2)))) false
                :else (recur (+ i 6)))))))

(defn solution
  "Returns the product a*b for the quadratic n^2+a*n+b with |a| < a-limit and
  b < b-limit producing the longest run of consecutive primes from n = 0."
  ([] (solution 1000 1000))
  ([a-limit] (solution a-limit 1000))
  ([a-limit b-limit]
   (let [[_ best-a best-b]
         (reduce (fn [[longest la lb :as acc] a]
                   (reduce (fn [[longest la lb :as acc2] b]
                             (if (not (prime? b))
                               acc2
                               (let [cnt (loop [n 0 c 0]
                                           (if (prime? (+ (* n n) (* a n) b))
                                             (recur (inc n) (inc c))
                                             c))]
                                 (if (> cnt longest) [cnt a b] acc2))))
                           acc
                           (range 2 b-limit)))
                 [0 0 0]
                 (range (inc (- a-limit)) a-limit))]
     (* best-a best-b))))
