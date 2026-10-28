(ns clojure-algorithms.project-euler.problem-025.sol1
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
  contain 1000 digits?")

(defn fibonacci
  "Computes the Fibonacci number for input n by iterating through n
  numbers and creating a vector of ints using the Fibonacci formula.
  Returns the nth element of the vector."
  [n]
  (cond (or (= n 1) (not (integer? n))) 0
        (= n 2) 1
        :else (let [sequence (loop [i 2 s [0 1]]
                               (if (> i n)
                                 s
                                 (recur (inc i)
                                        (conj s (+' (nth s (dec i))
                                                    (nth s (- i 2)))))))]
                (nth sequence n))))

(defn fibonacci-digits-index
  "Computes incrementing Fibonacci numbers until the length of the
  resulting Fibonacci number is the input value n. Returns the term of
  the Fibonacci sequence where this occurs."
  [n]
  (loop [digits 0 index 2]
    (if (>= digits n)
      index
      (let [index (inc index)]
        (recur (count (str (fibonacci index))) index)))))

(defn solution
  "Returns the index of the first term in the Fibonacci sequence to
  contain n digits."
  ([] (solution 1000))
  ([n]
   (fibonacci-digits-index n)))
