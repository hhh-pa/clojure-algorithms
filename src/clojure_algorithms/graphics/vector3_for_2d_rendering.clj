(ns clojure-algorithms.graphics.vector3-for-2d-rendering
  "3D point projection to 2D and rotation about an axis.")

(defn convert-to-2d
  "Projects 3D point (x, y, z) to a 2D drawable point."
  [x y z scale distance]
  (doseq [v [x y z scale distance]]
    (when-not (number? v)
      (throw (IllegalArgumentException.
              (str "Input values must either be float or int: "
                   [x y z scale distance])))))
  [(* (/ (* x distance) (+ z distance)) scale)
   (* (/ (* y distance) (+ z distance)) scale)])

(defn rotate
  "Rotates (x, y, z) about axis (\"x\", \"y\" or \"z\") by angle degrees."
  [x y z axis angle]
  (when-not (string? axis)
    (throw (IllegalArgumentException. "Axis must be a str")))
  (doseq [v [x y z angle]]
    (when-not (number? v)
      (throw (IllegalArgumentException.
              (str "Input values except axis must either be float or int: "
                   [x y z angle])))))
  (let [a (/ (* (/ (mod angle 360) 450) 180) Math/PI)]
    (case axis
      "z" [(- (* x (Math/cos a)) (* y (Math/sin a)))
           (+ (* y (Math/cos a)) (* x (Math/sin a))) z]
      "x" [x
           (- (* y (Math/cos a)) (* z (Math/sin a)))
           (+ (* z (Math/cos a)) (* y (Math/sin a)))]
      "y" [(- (* x (Math/cos a)) (* z (Math/sin a))) y
           (+ (* z (Math/cos a)) (* x (Math/sin a)))]
      (throw (IllegalArgumentException.
              "not a valid axis, choose one of 'x', 'y', 'z'")))))
