(ns clojure-algorithms.project-euler.problem-085.sol1
  "Project Euler 85: counting rectangles.

  For a grid with side-lengths a and b, the number of rectangles
  contained in the grid is [a*(a+1)/2] * [b*(b+1)/2], the product of the
  a-th and b-th triangle numbers. Although no rectangular grid contains
  exactly two million rectangles, find the area of the grid with the
  nearest solution.

  The best pair of triangle numbers is found in linear time: for each
  triangle number Ta, Tb must be roughly target/Ta, so b is estimated
  with the quadratic formula and only the integers above and below the
  estimate are tried.")

(defn solution
  "Finds the area of the grid whose rectangle count is closest to
  target."
  ([] (solution 2000000))
  ([target]
   (let [bound (long (Math/ceil (* (Math/sqrt (* target 2)) 1.1)))
         triangles (vec (reductions + 0 (range 1 bound)))]
     (loop [idx-a 1 best-product 0 area 0]
       (if (>= idx-a (count triangles))
         area
         (let [triangle-a (nth triangles idx-a)
               b-estimate (/ (+ -1 (Math/sqrt
                                    (+ 1 (/ (* 8.0 target) triangle-a))))
                             2)
               b-floor (long (Math/floor b-estimate))
               b-ceil (long (Math/ceil b-estimate))
               first-guess (nth triangles b-floor)
               second-guess (nth triangles b-ceil)
               [best-product area]
               (if (< (abs (- target (* first-guess triangle-a)))
                      (abs (- target best-product)))
                 [(* first-guess triangle-a) (* idx-a b-floor)]
                 [best-product area])
               [best-product area]
               (if (< (abs (- target (* second-guess triangle-a)))
                      (abs (- target best-product)))
                 [(* second-guess triangle-a) (* idx-a b-ceil)]
                 [best-product area])]
           (recur (inc idx-a) best-product area)))))))
