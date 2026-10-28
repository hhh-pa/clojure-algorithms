(ns clojure-algorithms.project-euler.problem-112.sol1
  "Project Euler Problem 112: bouncy numbers.

  Working from left-to-right if no digit is exceeded by the digit to its left
  it is called an increasing number; similarly a decreasing number reads
  non-increasing left-to-right. A positive integer that is neither increasing
  nor decreasing is a bouncy number. Finds the least number for which the
  proportion of bouncy numbers is exactly a given percent.")

(defn check-bouncy
  "Returns true if number n is bouncy, false otherwise."
  [n]
  (when-not (integer? n)
    (throw (IllegalArgumentException.
            "check-bouncy() accepts only integer arguments")))
  (let [s (str n)
        sorted-s (apply str (sort s))]
    (not (or (= s sorted-s) (= s (apply str (reverse sorted-s)))))))

(defn solution
  "Returns the least number for which the proportion of bouncy numbers is
  exactly percent."
  ([] (solution 99))
  ([percent]
   (when-not (< 0 percent 100)
     (throw (IllegalArgumentException.
             "solution() only accepts values from 0 to 100")))
   (loop [num 1
          bouncy-num 0]
     (let [bouncy-num (+ bouncy-num (if (check-bouncy num) 1 0))]
       (if (>= (* (/ (double bouncy-num) num) 100) percent)
         num
         (recur (inc num) bouncy-num))))))
