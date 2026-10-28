(ns clojure-algorithms.project-euler.problem-301.sol1
  "Project Euler Problem 301: Nim heaps.

  In the three-heap normal-play version of Nim, X(n1, n2, n3) returns zero
  if the player about to move will eventually lose with perfect strategy.
  Counts for how many positive integers n <= 2^30 X(n, 2n, 3n) = 0, which is
  the Fibonacci number F(exponent + 2).")

(defn solution
  "For a given exponent x >= 0 with 1 <= n <= 2^x, returns how many Nim
  games with heaps (n, 2n, 3n) are lost."
  ([] (solution 30))
  ([exponent]
   (let [fibonacci-index (+ exponent 2)
         phi (/ (+ 1 (Math/sqrt 5)) 2)
         fibonacci (/ (- (Math/pow phi fibonacci-index)
                         (Math/pow (- phi 1) fibonacci-index))
                      (Math/sqrt 5))]
     (long fibonacci))))
