(ns clojure-algorithms.project-euler.problem-025.sol2
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

  This solution draws Fibonacci numbers from a lazy sequence.")

(defn fibonacci-generator
  "Returns a lazy sequence of numbers in the Fibonacci sequence."
  []
  (map second (rest (iterate (fn [[a b]] [b (+' a b)]) [0 1]))))

(defn solution
  "Returns the index of the first term in the Fibonacci sequence to
  contain n digits."
  ([] (solution 1000))
  ([n]
   (loop [answer 1 gen (fibonacci-generator)]
     (if (>= (count (str (first gen))) n)
       (inc answer)
       (recur (inc answer) (rest gen))))))
