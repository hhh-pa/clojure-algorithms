(ns clojure-algorithms.project-euler.problem-024.sol1
  "Project Euler Problem 24: lexicographic permutations.

  A permutation is an ordered arrangement of objects. For example, 3124
  is one possible permutation of the digits 1, 2, 3 and 4. If all of the
  permutations are listed numerically or alphabetically, we call it
  lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

      012   021   102   120   201   210

  What is the millionth lexicographic permutation of the digits 0, 1, 2,
  3, 4, 5, 6, 7, 8 and 9?")

(defn- permutations
  "Returns a lazy sequence of the lexicographic permutations of coll,
  each represented as a seq of its elements."
  [coll]
  (let [coll (vec coll)]
    (if (empty? coll)
      (list '())
      (mapcat (fn [i]
                (map #(cons (nth coll i) %)
                     (permutations (concat (subvec coll 0 i)
                                           (subvec coll (inc i))))))
              (range (count coll))))))

(defn solution
  "Returns the n-th (1-based) lexicographic permutation of digits as a
  string. Defaults to the millionth permutation of 0123456789."
  ([] (solution "0123456789" 1000000))
  ([digits n]
   (apply str (nth (permutations (seq digits)) (dec n)))))
