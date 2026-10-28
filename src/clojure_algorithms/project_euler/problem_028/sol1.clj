(ns clojure-algorithms.project-euler.problem-028.sol1
  "Euler Problem 28: sum of the numbers on the diagonals in an n by n spiral
  formed by starting with 1 and moving clockwise outward.")

(defn solution
  "Returns the sum of the numbers on the diagonals in an n by n spiral."
  ([] (solution 1001))
  ([n]
   (loop [i 1 total 1]
     (if (>= i (Math/ceil (/ n 2.0)))
       total
       (let [odd (+ (* 2 i) 1)
             even (* 2 i)]
         (recur (inc i) (+ total (- (* 4 (* odd odd)) (* 6 even)))))))))
