(ns clojure-algorithms.project-euler.problem-037.sol1
  "Euler Problem 37: find the sum of the only eleven primes that are both
  truncatable from left to right and right to left.")

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

(defn list-truncated-nums
  "Returns a vector of all left- and right-truncated numbers of n, starting
  with n itself."
  [n]
  (let [s (str n)]
    (into [n]
          (mapcat (fn [i]
                    [(Integer/parseInt (subs s i))
                     (Integer/parseInt (subs s 0 (- (count s) i)))])
                  (range 1 (count s))))))

(defn validate
  "Rules out numbers above 1000 whose first or last three digits are not
  prime, as an optimization."
  [n]
  (let [s (str n)]
    (not (and (> (count s) 3)
              (or (not (prime? (Integer/parseInt (subs s (- (count s) 3)))))
                  (not (prime? (Integer/parseInt (subs s 0 3)))))))))

(defn compute-truncated-primes
  "Returns the vector of the first n truncatable primes."
  ([] (compute-truncated-primes 11))
  ([n]
   (loop [found [] num 13]
     (if (= (count found) n)
       found
       (recur (if (and (validate num)
                       (every? prime? (list-truncated-nums num)))
                (conj found num)
                found)
              (+ num 2))))))

(defn solution
  "Returns the sum of the eleven truncatable primes."
  []
  (reduce + (compute-truncated-primes 11)))
