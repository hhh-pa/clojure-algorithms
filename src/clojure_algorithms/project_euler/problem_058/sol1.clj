(ns clojure-algorithms.project-euler.problem-058.sol1
  "Euler Problem 58: what is the side length of the square spiral for which the
  ratio of primes along both diagonals first falls below 10%?")

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
  "Returns the odd side length greater than 1 for which the ratio of primes
  along both diagonals of the square spiral first falls below ratio."
  ([] (solution 0.1))
  ([ratio]
   (loop [j 3 primes 3]
     (if (< (/ primes (double (dec (* 2 j)))) ratio)
       j
       (let [new-primes (count (filter prime?
                                       (range (+ (* j j) j 1)
                                              (* (+ j 2) (+ j 2))
                                              (+ j 1))))]
         (recur (+ j 2) (+ primes new-primes)))))))
