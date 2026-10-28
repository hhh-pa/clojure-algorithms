(ns clojure-algorithms.project-euler.problem-091.sol1
  "Project Euler 91: right triangles with integer coordinates.

  The points P (x1, y1) and Q (x2, y2) are plotted at integer
  coordinates and are joined to the origin, O(0,0), to form a triangle.
  Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be
  formed?")

(defn is-right
  "Checks if the triangle described by P(x1,y1), Q(x2,y2) and O(0,0) is
  right-angled. Does not check whether P and Q are equal; that is
  handled by enumerating unordered pairs in solution."
  [x1 y1 x2 y2]
  (if (or (and (zero? x1) (zero? y1)) (and (zero? x2) (zero? y2)))
    false
    (let [a-square (+ (* x1 x1) (* y1 y1))
          b-square (+ (* x2 x2) (* y2 y2))
          c-square (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))]
      (or (= (+ a-square b-square) c-square)
          (= (+ a-square c-square) b-square)
          (= (+ b-square c-square) a-square)))))

(defn solution
  "Returns the number of right triangles OPQ that can be formed by two
  points P, Q which have both x- and y-coordinates between 0 and limit
  inclusive."
  ([] (solution 50))
  ([limit]
   (let [points (vec (for [x (range (inc limit)) y (range (inc limit))]
                       [x y]))
         n (count points)]
     (count (for [i (range n)
                  j (range (inc i) n)
                  :let [[[x1 y1] [x2 y2]] [(nth points i) (nth points j)]]
                  :when (is-right x1 y1 x2 y2)]
              1)))))
