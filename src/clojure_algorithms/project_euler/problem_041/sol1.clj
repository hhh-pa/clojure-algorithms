(ns clojure-algorithms.project-euler.problem-041.sol1
  "Euler Problem 41: what is the largest n-digit pandigital prime that exists?
  All pandigital numbers except 1, 4 and 7 digit ones are divisible by 3, so
  the search focuses on 7-digit pandigitals.")

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

(defn- permutations
  "Returns a lazy sequence of all permutations of the string s as strings."
  [s]
  (let [v (vec s)]
    (if (<= (count v) 1)
      (list (apply str v))
      (for [i (range (count v))
            rest-p (permutations (apply str (into (subvec v 0 i)
                                                  (subvec v (inc i)))))]
        (str (v i) rest-p)))))

(defn solution
  "Returns the maximum pandigital prime number of length n, or 0 if none
  exists."
  ([] (solution 7))
  ([n]
   (let [digits (apply str (map inc (range n)))
         pandigitals (filter prime?
                             (map #(Integer/parseInt %) (permutations digits)))]
     (if (seq pandigitals) (apply max pandigitals) 0))))
