(ns clojure-algorithms.project-euler.problem-207.sol1
  "Project Euler Problem 207: integer partitions of powers.

  For some positive integers k there is an integer partition of the form
  4^t = 2^t + k. Partitions where t is also an integer are called perfect.
  For m >= 1, P(m) is the proportion of such partitions that are perfect
  with k <= m. Finds the smallest m for which P(m) < 1/12345.")

(defn check-partition-perfect
  "Checks if t = log2(sqrt(4*k+1)/2 + 1/2) is an integer for the given
  positive integer k. Uses exact integer arithmetic: t is an integer exactly
  when 4k+1 is a perfect square s*s and (s+1)/2 is a power of two."
  [positive-integer]
  (let [target (inc (* 4 positive-integer))
        s (long (Math/sqrt target))
        v (quot (inc s) 2)]
    (boolean (and (= (* s s) target)
                  (pos? v)
                  (zero? (bit-and v (dec v)))))))

(defn solution
  "Finds m for which the proportion of perfect partitions to total
  partitions is lower than max-proportion."
  ([] (solution (/ 1.0 12345)))
  ([max-proportion]
   (loop [integer 3
          total-partitions 0
          perfect-partitions 0
          candidate 0]
     (let [partition-candidate (/ (- (* integer integer) 1) 4.0)
           is-partition (== partition-candidate (long partition-candidate))
           partition-int (if is-partition (long partition-candidate) candidate)
           total-partitions (if is-partition
                              (inc total-partitions)
                              total-partitions)
           perfect-partitions (if (and is-partition
                                       (check-partition-perfect partition-int))
                                (inc perfect-partitions)
                                perfect-partitions)]
       (if (and (pos? perfect-partitions)
                (< (/ (double perfect-partitions)
                      (double total-partitions))
                   (double max-proportion)))
         partition-int
         (recur (inc integer)
                total-partitions
                perfect-partitions
                partition-int))))))
