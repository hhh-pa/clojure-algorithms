(ns clojure-algorithms.project-euler.problem-046.sol1
  "Euler Problem 46: Goldbach's other conjecture (every odd composite is the
  sum of a prime and twice a square) is false. Find the smallest odd composite
  that cannot be written that way.")

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

(def odd-composites
  "All odd composite numbers below 100001."
  (vec (filter (fn [num] (not (prime? num))) (range 3 100001 2))))

(defn- follows-conjecture?
  "Returns true if num can be written as a prime plus twice a square."
  [num]
  (loop [i 0]
    (cond
      (> (* 2 i i) num) false
      (prime? (- num (* 2 i i))) true
      :else (recur (inc i)))))

(defn compute-nums
  "Returns a vector of the first n odd composite numbers which do not follow
  the conjecture. Throws IllegalArgumentException unless n is a positive
  integer."
  [n]
  (when (not (integer? n))
    (throw (IllegalArgumentException. "n must be an integer")))
  (when (<= n 0)
    (throw (IllegalArgumentException. "n must be >= 0")))
  (vec (take n (remove follows-conjecture? odd-composites))))

(defn solution
  "Returns the smallest odd composite that is not the sum of a prime and twice
  a square."
  []
  (first (compute-nums 1)))
