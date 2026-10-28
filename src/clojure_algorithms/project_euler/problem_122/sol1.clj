(ns clojure-algorithms.project-euler.problem-122.sol1
  "Project Euler Problem 122: efficient exponentiation.

  Defines m(k) to be the minimum number of multiplications to compute n^k;
  for example m(15) = 5. Uses the fact that for rather small n the solution
  for each number can be formed by increasing the largest element. Finds the
  sum of m(k) for k from 1 to 200.")

(defn solve
  "Checks if nums can have a sum equal to goal, given that the length of
  nums does not exceed depth."
  [nums goal depth]
  (if (> (count nums) depth)
    false
    (boolean (some (fn [el]
                     (let [s (+ el (peek nums))]
                       (if (= s goal)
                         true
                         (solve (conj nums s) goal depth))))
                   nums))))

(defn solution
  "Calculates the sum of the smallest number of multiplications for each
  number up to and including n."
  ([] (solution 200))
  ([n]
   (reduce + 0
           (for [i (range 2 (inc n))]
             (loop [max-length 1]
               (if (solve [1] i max-length)
                 max-length
                 (recur (inc max-length))))))))
