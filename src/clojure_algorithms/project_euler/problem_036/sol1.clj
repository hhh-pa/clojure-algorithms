(ns clojure-algorithms.project-euler.problem-036.sol1
  "Euler Problem 36: find the sum of all numbers less than one million which
  are palindromic in base 10 and base 2.")

(defn palindrome?
  "Returns true if the input n (an integer or a string) is a palindrome."
  [n]
  (let [s (str n)]
    (= s (apply str (reverse s)))))

(defn solution
  "Returns the sum of all numbers less than n which are palindromic in base 10
  and base 2."
  ([] (solution 1000000))
  ([n]
   (reduce + 0 (filter (fn [i]
                         (and (palindrome? i)
                              (palindrome? (Integer/toBinaryString i))))
                       (range 1 n)))))
