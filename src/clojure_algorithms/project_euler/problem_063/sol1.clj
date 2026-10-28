(ns clojure-algorithms.project-euler.problem-063.sol1
  "Project Euler 63: powerful digit counts.

  The 5-digit number, 16807 = 7^5, is also a fifth power. Similarly, the
  9-digit number, 134217728 = 8^9, is a ninth power. How many n-digit
  positive integers exist which are also an nth power?

  The maximum base can be 9 because all n-digit numbers are < 10^n.
  9^23 has 22 digits so the maximum power can be 22.")

(defn solution
  "Returns the count of all n-digit numbers which are nth powers,
  for bases in [1, max-base) and powers in [1, max-power)."
  ([] (solution 10 22))
  ([max-base] (solution max-base 22))
  ([max-base max-power]
   (count (for [power (range 1 max-power)
                base (range 1 max-base)
                :when (= (count (str (.pow (biginteger base) power)))
                         power)]
            1))))
