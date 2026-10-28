(ns clojure-algorithms.maths.area
  "Find the area of various geometric shapes.
Wikipedia reference: https://en.wikipedia.org/wiki/Area")

(defn- non-negative?
  "Returns true when every value in xs is non-negative."
  [& xs]
  (every? #(not (neg? %)) xs))

(defn surface-area-cube
  "Calculate the Surface Area of a Cube."
  [side-length]
  (when (neg? side-length)
    (throw (ex-info "surface_area_cube() only accepts non-negative values"
                    {:type :ValueError})))
  (* 6 side-length side-length))

(defn surface-area-cuboid
  "Calculate the Surface Area of a Cuboid."
  [length breadth height]
  (when-not (non-negative? length breadth height)
    (throw (ex-info "surface_area_cuboid() only accepts non-negative values"
                    {:type :ValueError})))
  (* 2 (+ (* length breadth) (* breadth height) (* length height))))

(defn surface-area-sphere
  "Calculate the Surface Area of a Sphere.
Wikipedia reference: https://en.wikipedia.org/wiki/Sphere
Formula: 4 * pi * r^2"
  [radius]
  (when (neg? radius)
    (throw (ex-info "surface_area_sphere() only accepts non-negative values"
                    {:type :ValueError})))
  (* 4 Math/PI radius radius))

(defn surface-area-hemisphere
  "Calculate the Surface Area of a Hemisphere.
Formula: 3 * pi * r^2"
  [radius]
  (when (neg? radius)
    (throw (ex-info "surface_area_hemisphere() only accepts non-negative values"
                    {:type :ValueError})))
  (* 3 Math/PI radius radius))

(defn surface-area-cone
  "Calculate the Surface Area of a Cone.
Wikipedia reference: https://en.wikipedia.org/wiki/Cone
Formula: pi * r * (r + (h ** 2 + r ** 2) ** 0.5)"
  [radius height]
  (when-not (non-negative? radius height)
    (throw (ex-info "surface_area_cone() only accepts non-negative values"
                    {:type :ValueError})))
  (* Math/PI radius (+ radius (Math/pow (+ (* height height) (* radius radius)) 0.5))))

(defn lateral-surface-cone
  "Calculate the Lateral Surface Area of a Cone.
Wikipedia reference: https://en.wikipedia.org/wiki/Cone
Formula: pi * r * (h ** 2 + r ** 2) ** 0.5"
  [radius height]
  (when-not (non-negative? radius height)
    (throw (ex-info "lateral_surface_cone() only accepts non-negative values"
                    {:type :ValueError})))
  (* Math/PI radius (Math/pow (+ (* height height) (* radius radius)) 0.5)))

(defn surface-area-conical-frustum
  "Calculate the Surface Area of a Conical Frustum."
  [radius-1 radius-2 height]
  (when-not (non-negative? radius-1 radius-2 height)
    (throw (ex-info "surface_area_conical_frustum() only accepts non-negative values"
                    {:type :ValueError})))
  (let [slant-height (Math/pow (+ (* height height)
                                  (* (- radius-1 radius-2) (- radius-1 radius-2)))
                               0.5)]
    (* Math/PI (+ (* slant-height (+ radius-1 radius-2))
                  (* radius-1 radius-1)
                  (* radius-2 radius-2)))))

(defn surface-area-cylinder
  "Calculate the Surface Area of a Cylinder.
Wikipedia reference: https://en.wikipedia.org/wiki/Cylinder
Formula: 2 * pi * r * (h + r)"
  [radius height]
  (when-not (non-negative? radius height)
    (throw (ex-info "surface_area_cylinder() only accepts non-negative values"
                    {:type :ValueError})))
  (* 2 Math/PI radius (+ height radius)))

(defn lateral-surface-cylinder
  "Calculate the Lateral Surface Area of a Cylinder.
Wikipedia reference: https://en.wikipedia.org/wiki/Cylinder
Formula: 2 * pi * r * h"
  [radius height]
  (when-not (non-negative? radius height)
    (throw (ex-info "lateral_surface_cylinder() only accepts non-negative values"
                    {:type :ValueError})))
  (* 2 Math/PI radius height))

(defn surface-area-torus
  "Calculate the Area of a Torus.
Wikipedia reference: https://en.wikipedia.org/wiki/Torus
Returns 4pi^2 * torus_radius * tube_radius."
  [torus-radius tube-radius]
  (when-not (non-negative? torus-radius tube-radius)
    (throw (ex-info "surface_area_torus() only accepts non-negative values"
                    {:type :ValueError})))
  (when (< torus-radius tube-radius)
    (throw (ex-info "surface_area_torus() does not support spindle or self intersecting tori"
                    {:type :ValueError})))
  (* 4 (Math/pow Math/PI 2) torus-radius tube-radius))

(defn area-rectangle
  "Calculate the area of a rectangle."
  [length width]
  (when-not (non-negative? length width)
    (throw (ex-info "area_rectangle() only accepts non-negative values"
                    {:type :ValueError})))
  (* length width))

(defn area-square
  "Calculate the area of a square."
  [side-length]
  (when (neg? side-length)
    (throw (ex-info "area_square() only accepts non-negative values"
                    {:type :ValueError})))
  (* side-length side-length))

(defn area-triangle
  "Calculate the area of a triangle given the base and height."
  [base height]
  (when-not (non-negative? base height)
    (throw (ex-info "area_triangle() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* base height) 2.0))

(defn area-triangle-three-sides
  "Calculate area of triangle when the length of 3 sides are known.
Uses Heron's formula: https://en.wikipedia.org/wiki/Heron%27s_formula"
  [side1 side2 side3]
  (when-not (non-negative? side1 side2 side3)
    (throw (ex-info "area_triangle_three_sides() only accepts non-negative values"
                    {:type :ValueError})))
  (when (or (< (+ side1 side2) side3)
            (< (+ side1 side3) side2)
            (< (+ side2 side3) side1))
    (throw (ex-info "Given three sides do not form a triangle" {:type :ValueError})))
  (let [semi-perimeter (/ (+ side1 side2 side3) 2.0)]
    (Math/sqrt (* semi-perimeter
                  (- semi-perimeter side1)
                  (- semi-perimeter side2)
                  (- semi-perimeter side3)))))

(defn area-parallelogram
  "Calculate the area of a parallelogram."
  [base height]
  (when-not (non-negative? base height)
    (throw (ex-info "area_parallelogram() only accepts non-negative values"
                    {:type :ValueError})))
  (* base height))

(defn area-trapezium
  "Calculate the area of a trapezium."
  [base1 base2 height]
  (when-not (non-negative? base1 base2 height)
    (throw (ex-info "area_trapezium() only accepts non-negative values"
                    {:type :ValueError})))
  (* 0.5 (+ base1 base2) height))

(defn area-circle
  "Calculate the area of a circle."
  [radius]
  (when (neg? radius)
    (throw (ex-info "area_circle() only accepts non-negative values"
                    {:type :ValueError})))
  (* Math/PI radius radius))

(defn area-ellipse
  "Calculate the area of an ellipse."
  [radius-x radius-y]
  (when-not (non-negative? radius-x radius-y)
    (throw (ex-info "area_ellipse() only accepts non-negative values"
                    {:type :ValueError})))
  (* Math/PI radius-x radius-y))

(defn area-rhombus
  "Calculate the area of a rhombus."
  [diagonal-1 diagonal-2]
  (when-not (non-negative? diagonal-1 diagonal-2)
    (throw (ex-info "area_rhombus() only accepts non-negative values"
                    {:type :ValueError})))
  (* 0.5 diagonal-1 diagonal-2))

(defn area-reg-polygon
  "Calculate the area of a regular polygon.
Wikipedia reference: https://en.wikipedia.org/wiki/Polygon#Regular_polygons
Formula: (n*s^2*cot(pi/n))/4"
  [sides length]
  (when (or (not (integer? sides)) (< sides 3))
    (throw (ex-info "area_reg_polygon() only accepts integers greater than or equal to three as number of sides"
                    {:type :ValueError})))
  (when (neg? length)
    (throw (ex-info "area_reg_polygon() only accepts non-negative values as length of a side"
                    {:type :ValueError})))
  (/ (* sides length length) (* 4 (Math/tan (/ Math/PI (double sides))))))

(defn surface-area-reg-prism
  "Calculate the surface area of a regular prism.
Wikipedia reference: https://en.wikipedia.org/wiki/Prism
Formula: 2*S(base) + n*a*h"
  [sides edge height]
  (when (or (not (integer? sides)) (< sides 3))
    (throw (ex-info "surface_area_reg_prism() only accepts integers greater than or equal to three as number of sides"
                    {:type :ValueError})))
  (when-not (non-negative? edge height)
    (throw (ex-info "surface_area_reg_prism() only accepts non-negative values as length of an edge or height"
                    {:type :ValueError})))
  (+ (* 2 (area-reg-polygon sides edge)) (* sides edge height)))
