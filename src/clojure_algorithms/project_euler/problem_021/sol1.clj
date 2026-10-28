(ns clojure-algorithms.project-euler.problem-021.sol1
  "Project Euler Problem 21: amicable numbers.

  Let d(n) be defined as the sum of proper divisors of n (numbers less
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a != b, then a and b are an amicable
  pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
  22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
  284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.")

(defn sum-of-divisors
  "Returns the sum of the proper divisors of n (divisors less than n)."
  [n]
  (let [root (Math/sqrt (double n))]
    (loop [i 1 total 0]
      (if (>= i (long (inc root)))
        (- total n)
        (recur (inc i)
               (cond (and (zero? (mod n i)) (not (== i root)))
                     (+ total i (quot n i))
                     (== i root)
                     (+ total i)
                     :else total))))))

(defn solution
  "Returns the sum of all the amicable numbers under n."
  ([] (solution 10000))
  ([n]
   (reduce + 0
           (filter (fn [i]
                     (let [s (sum-of-divisors i)]
                       (and (= (sum-of-divisors s) i) (not= s i))))
                   (range 1 n)))))
