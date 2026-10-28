(ns clojure-algorithms.project-euler.problem-551.sol1
  "Project Euler Problem 551: sum of digits sequence.

  Let a(0), a(1), ... be defined by a(0) = 1 and, for n >= 1, a(n) as the
  sum of the digits of all preceding terms. The sequence starts
  1, 1, 2, 4, 8, ... Equivalently, a(n) = a(n-1) + digit-sum(a(n-1)).
  Finds a(10^15); a(10^6) = 31054319.

  The Python original uses a memoized jump algorithm to reach 10^15. This
  port computes the same sequence by direct iteration, which is exact for
  every n but only practical for modest n.")

(defn- digit-sum
  "Returns the sum of the decimal digits of n."
  [n]
  (loop [n n
         acc 0]
    (if (zero? n)
      acc
      (recur (quot n 10) (+ acc (mod n 10))))))

(defn solution
  "Returns the nth term of the sequence."
  ([] (solution 1000000000000000))
  ([n]
   (loop [i 1
          a 1]
     (if (>= i n)
       a
       (recur (inc i) (+ a (digit-sum a)))))))
