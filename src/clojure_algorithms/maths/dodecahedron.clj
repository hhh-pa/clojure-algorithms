(ns clojure-algorithms.maths.dodecahedron
  "A regular dodecahedron is a three-dimensional figure made up of
  12 pentagon faces having the same equal size.")

(defn- check-edge
  [edge]
  ;; Note: upstream also rejects non-integer edges
  ;; (`not isinstance(edge, int)`); that quirk is preserved here.
  (when (or (<= edge 0) (not (integer? edge)))
    (throw (ex-info "Length must be a positive." {:type :ValueError}))))

(defn dodecahedron-surface-area
  "Calculates the surface area of a regular dodecahedron:
  a = 3 * sqrt(25 + 10 * sqrt(5)) * e^2.
  Reference: \"Dodecahedron\" Study.com
  <https://study.com/academy/lesson/dodecahedron-volume-surface-area-formulas.html>"
  [edge]
  (check-edge edge)
  (* 3 (Math/sqrt (+ 25 (* 10 (Math/sqrt 5)))) edge edge))

(defn dodecahedron-volume
  "Calculates the volume of a regular dodecahedron:
  v = ((15 + 7 * sqrt(5)) / 4) * e^3.
  Reference: \"Dodecahedron\" Study.com
  <https://study.com/academy/lesson/dodecahedron-volume-surface-area-formulas.html>"
  [edge]
  (check-edge edge)
  (* (/ (+ 15 (* 7 (Math/sqrt 5))) 4) edge edge edge))
