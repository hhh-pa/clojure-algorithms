(ns clojure-algorithms.project-euler.problem-115.sol1
  "Project Euler Problem 115: fill-count function.

  A harder version of Problem 114. A row measuring n units in length has red
  blocks with a minimum length of m units placed on it. Let the fill-count
  function F(m, n) represent the number of ways that a row can be filled. For
  m = 50, finds the least value of n for which the fill-count function first
  exceeds one million.")

(defn solution
  "Returns, for the given minimum block length, the least value of n for
  which the fill-count function first exceeds one million."
  ([] (solution 50))
  ([min-block-length]
   (loop [n min-block-length
          fill-counts (vec (repeat min-block-length 1))]
     (let [value (+ 1 (reduce + 0
                              (for [block-length (range min-block-length (inc n))]
                                (+ 1 (reduce + 0
                                             (for [block-start (range (- n block-length))]
                                               (nth fill-counts
                                                    (- n block-start block-length 1))))))))]
       (if (> value 1000000)
         n
         (recur (inc n) (conj fill-counts value)))))))
