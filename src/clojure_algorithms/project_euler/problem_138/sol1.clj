(ns clojure-algorithms.project-euler.problem-138.sol1
  "Project Euler Problem 138: special isosceles triangles.

  With c = b/2 and h = 2c +- 1, the triangle relation c^2 + h^2 = L^2 becomes
  5L^2 - 1 = m^2, a negative Pell equation with n = 5 that is solved
  recursively. The first solution (m = 2, L = 1) is skipped since it gives
  non-integer b and h.")

(defn solution
  "Sums k + 1 values of L from the recursive solution of the negative Pell
  equation, skipping the first solution."
  ([] (solution 12))
  ([k]
   (loop [i 2
          m-i 2
          l-i 1
          ans 0]
     (if (> i (inc k))
       ans
       (let [new-m (+ (* 4 m-i) (* 5 m-i) (* 20 l-i))
             new-l (+ (* 4 l-i) (* 5 l-i) (* 4 m-i))]
         (recur (inc i) new-m new-l (+ ans new-l)))))))
