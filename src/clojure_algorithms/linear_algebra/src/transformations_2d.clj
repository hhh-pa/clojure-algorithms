(ns clojure-algorithms.linear-algebra.src.transformations-2d
  "2D transformation matrices: scaling, rotation, projection, reflection.")

(defn scaling
  "Returns the 2x2 uniform-scaling matrix for scaling-factor."
  [scaling-factor]
  (let [s (double scaling-factor)]
    [[s 0.0] [0.0 s]]))

(defn rotation
  "Returns the 2x2 rotation matrix for angle (radians)."
  [angle]
  (let [c (Math/cos angle)
        s (Math/sin angle)]
    [[c (- s)] [s c]]))

(defn projection
  "Returns the 2x2 projection matrix for angle (radians)."
  [angle]
  (let [c (Math/cos angle)
        s (Math/sin angle)
        cs (* c s)]
    [[(* c c) cs] [cs (* s s)]]))

(defn reflection
  "Returns the 2x2 reflection matrix for angle (radians),
  following the Python original's formula."
  [angle]
  (let [c (Math/cos angle)
        s (Math/sin angle)
        cs (* c s)]
    [[(dec (* 2 c)) (* 2 cs)] [(* 2 cs) (dec (* 2 s))]]))
