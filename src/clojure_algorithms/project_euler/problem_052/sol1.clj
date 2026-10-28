(ns clojure-algorithms.project-euler.problem-052.sol1
  "Euler Problem 52: find the smallest positive integer x such that 2x, 3x,
  4x, 5x and 6x contain the same digits.")

(defn solution
  "Returns the smallest positive integer whose multiples 2x through 6x are
  digit permutations of x."
  []
  (loop [i 1]
    (let [base (sort (str i))]
      (if (every? (fn [m] (= base (sort (str (* m i))))) [2 3 4 5 6])
        i
        (recur (inc i))))))
