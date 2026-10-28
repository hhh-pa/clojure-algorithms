(ns clojure-algorithms.project-euler.problem-144.sol1
  "White cell laser reflections.

  A white cell is an ellipse with equation 4x^2 + y^2 = 100 where the section
  -0.01 <= x <= +0.01 at the top is missing. A beam starts at (0.0, 10.1)
  and first impacts the mirror at (1.4, -9.6), following the law of
  reflection at each bounce. Counts how many times the beam hits the internal
  surface before exiting.")

(defn next-point
  "Given that a laser beam hits the interior of the white cell at point
  (point-x, point-y) with gradient incoming-gradient, returns a vector
  [x y m1] where the next point of contact is (x, y) with gradient m1."
  [point-x point-y incoming-gradient]
  (let [normal-gradient (/ (/ point-y 4) point-x)
        s2 (/ (* 2 normal-gradient) (+ 1 (* normal-gradient normal-gradient)))
        c2 (/ (- 1 (* normal-gradient normal-gradient))
              (+ 1 (* normal-gradient normal-gradient)))
        outgoing-gradient (/ (- s2 (* c2 incoming-gradient))
                             (+ c2 (* s2 incoming-gradient)))
        quadratic-term (+ (* outgoing-gradient outgoing-gradient) 4)
        linear-term (* 2
                       outgoing-gradient
                       (- point-y (* outgoing-gradient point-x)))
        constant-term (- (Math/pow (- point-y (* outgoing-gradient point-x)) 2)
                         100)
        discriminant (- (* linear-term linear-term)
                        (* 4 quadratic-term constant-term))
        x-minus (/ (- (- linear-term) (Math/sqrt discriminant))
                   (* 2 quadratic-term))
        x-plus (/ (+ (- linear-term) (Math/sqrt discriminant))
                  (* 2 quadratic-term))
        tolerance (* 1e-9 (max 1.0
                               (Math/abs x-plus)
                               (Math/abs point-x)))
        next-x (if (< (Math/abs (- x-plus point-x)) tolerance)
                 x-minus
                 x-plus)
        next-y (+ point-y (* outgoing-gradient (- next-x point-x)))]
    [next-x next-y outgoing-gradient]))

(defn solution
  "Returns the number of times that the beam hits the interior wall of the
  cell before exiting."
  ([] (solution 1.4 -9.6))
  ([first-x-coord first-y-coord]
   (loop [point-x first-x-coord
          point-y first-y-coord
          gradient (/ (- 10.1 first-y-coord) (- 0.0 first-x-coord))
          num-reflections 0]
     (if (and (<= -0.01 point-x 0.01) (pos? point-y))
       num-reflections
       (let [[next-x next-y next-gradient]
             (next-point point-x point-y gradient)]
         (recur next-x next-y next-gradient (inc num-reflections)))))))
