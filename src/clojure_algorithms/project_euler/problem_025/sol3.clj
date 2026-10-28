(ns clojure-algorithms.project-euler.problem-025.sol3
  "Project Euler Problem 25: 1000-digit Fibonacci number.

  The Fibonacci sequence is defined by the recurrence relation:

      Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.

  Hence the first 12 terms will be:

      F1 = 1
      F2 = 1
      F3 = 2
      F4 = 3
      F5 = 5
      F6 = 8
      F7 = 13
      F8 = 21
      F9 = 34
      F10 = 55
      F11 = 89
      F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the index of the first term in the Fibonacci sequence to
  contain 1000 digits?

  This solution iterates the Fibonacci recurrence until the digit
  length matches n.")

(defn solution
  "Returns the index of the first term in the Fibonacci sequence to
  contain n digits."
  ([] (solution 1000))
  ([n]
   (loop [f1 1 f2 1 index 2]
     (let [f (+' f1 f2)]
       (if (= n (count (str f)))
         (inc index)
         (recur f2 f (inc index)))))))
