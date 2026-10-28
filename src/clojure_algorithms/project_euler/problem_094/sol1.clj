(ns clojure-algorithms.project-euler.problem-094.sol1
  "Project Euler 94: almost equilateral triangles.

  We shall define an almost equilateral triangle to be a triangle for
  which two sides are equal and the third differs by no more than one
  unit. Find the sum of the perimeters of all almost equilateral
  triangles with integral side lengths and area and whose perimeters do
  not exceed one billion.")

(defn solution
  "Returns the sum of the perimeters of all almost equilateral triangles
  with integral side lengths and area and whose perimeters do not exceed
  max-perimeter."
  ([] (solution 1000000000))
  ([max-perimeter]
   (loop [prev-value 1 value 2 perimeters-sum 0 i 0 perimeter 0]
     (if (> perimeter max-perimeter)
       perimeters-sum
       (let [perimeters-sum (+ perimeters-sum perimeter)
             prev-value (+ prev-value (* 2 value))
             value (+ value prev-value)
             perimeter (if (even? i)
                         (+ (* 2 value) 2)
                         (- (* 2 value) 2))]
         (recur prev-value value perimeters-sum (inc i) perimeter))))))
