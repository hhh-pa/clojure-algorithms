(ns clojure-algorithms.maths.volume
  "Find the volume of various shapes.

  * https://en.wikipedia.org/wiki/Volume
  * https://en.wikipedia.org/wiki/Spherical_cap")

(defn vol-cube
  "Calculate the Volume of a Cube."
  [side-length]
  (when (neg? side-length)
    (throw (ex-info "vol_cube() only accepts non-negative values"
                    {:type :ValueError})))
  (Math/pow side-length 3))

(defn vol-spherical-cap
  "Calculate the volume of the spherical cap."
  [height radius]
  (when (or (neg? height) (neg? radius))
    (throw (ex-info "vol_spherical_cap() only accepts non-negative values"
                    {:type :ValueError})))
  (* (/ 1 3.0) Math/PI (Math/pow height 2) (- (* 3 radius) height)))

(defn vol-sphere
  "Calculate the Volume of a Sphere."
  [radius]
  (when (neg? radius)
    (throw (ex-info "vol_sphere() only accepts non-negative values"
                    {:type :ValueError})))
  (* (/ 4 3.0) Math/PI (Math/pow radius 3)))

(defn vol-spheres-intersect
  "Calculate the volume of the intersection of two spheres."
  [radius-1 radius-2 centers-distance]
  (when (or (neg? radius-1) (neg? radius-2) (neg? centers-distance))
    (throw (ex-info "vol_spheres_intersect() only accepts non-negative values"
                    {:type :ValueError})))
  (if (zero? centers-distance)
    (vol-sphere (min radius-1 radius-2))
    (let [h1 (/ (* (+ (- radius-1 radius-2) centers-distance)
                   (- (+ radius-1 radius-2) centers-distance))
                (* 2 (double centers-distance)))
          h2 (/ (* (+ (- radius-2 radius-1) centers-distance)
                   (- (+ radius-2 radius-1) centers-distance))
                (* 2 (double centers-distance)))]
      (+ (vol-spherical-cap h1 radius-2) (vol-spherical-cap h2 radius-1)))))

(defn vol-spheres-union
  "Calculate the volume of the union of two spheres that possibly intersect."
  [radius-1 radius-2 centers-distance]
  (when (or (not (number? radius-1))
            (not (number? radius-2))
            (not (number? centers-distance)))
    (throw (ex-info "'<=' not supported between instances of non-numeric and numeric types"
                    {:type :TypeError})))
  (when (or (<= radius-1 0) (<= radius-2 0) (neg? centers-distance))
    (throw (ex-info (str "vol_spheres_union() only accepts non-negative values, "
                         "non-zero radius")
                    {:type :ValueError})))
  (if (zero? centers-distance)
    (vol-sphere (max radius-1 radius-2))
    (- (+ (vol-sphere radius-1) (vol-sphere radius-2))
       (vol-spheres-intersect radius-1 radius-2 centers-distance))))

(defn vol-cuboid
  "Calculate the Volume of a Cuboid."
  [width height length]
  (when (or (neg? width) (neg? height) (neg? length))
    (throw (ex-info "vol_cuboid() only accepts non-negative values"
                    {:type :ValueError})))
  (double (* width height length)))

(defn vol-cone
  "Calculate the Volume of a Cone."
  [area-of-base height]
  (when (or (neg? height) (neg? area-of-base))
    (throw (ex-info "vol_cone() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* area-of-base height) 3.0))

(defn vol-right-circ-cone
  "Calculate the Volume of a Right Circular Cone."
  [radius height]
  (when (or (neg? height) (neg? radius))
    (throw (ex-info "vol_right_circ_cone() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* Math/PI (Math/pow radius 2) height) 3.0))

(defn vol-prism
  "Calculate the Volume of a Prism."
  [area-of-base height]
  (when (or (neg? height) (neg? area-of-base))
    (throw (ex-info "vol_prism() only accepts non-negative values"
                    {:type :ValueError})))
  (double (* area-of-base height)))

(defn vol-pyramid
  "Calculate the Volume of a Pyramid."
  [area-of-base height]
  (when (or (neg? height) (neg? area-of-base))
    (throw (ex-info "vol_pyramid() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* area-of-base height) 3.0))

(defn vol-hemisphere
  "Calculate the volume of a hemisphere."
  [radius]
  (when (neg? radius)
    (throw (ex-info "vol_hemisphere() only accepts non-negative values"
                    {:type :ValueError})))
  (* (Math/pow radius 3) Math/PI (/ 2 3.0)))

(defn vol-circular-cylinder
  "Calculate the Volume of a Circular Cylinder."
  [radius height]
  (when (or (neg? height) (neg? radius))
    (throw (ex-info "vol_circular_cylinder() only accepts non-negative values"
                    {:type :ValueError})))
  (* (Math/pow radius 2) height Math/PI))

(defn vol-hollow-circular-cylinder
  "Calculate the Volume of a Hollow Circular Cylinder."
  [inner-radius outer-radius height]
  (when (or (neg? inner-radius) (neg? outer-radius) (neg? height))
    (throw (ex-info "vol_hollow_circular_cylinder() only accepts non-negative values"
                    {:type :ValueError})))
  (when (<= outer-radius inner-radius)
    (throw (ex-info "outer_radius must be greater than inner_radius"
                    {:type :ValueError})))
  (* Math/PI (- (Math/pow outer-radius 2) (Math/pow inner-radius 2)) height))

(defn vol-conical-frustum
  "Calculate the Volume of a Conical Frustum."
  [height radius-1 radius-2]
  (when (or (neg? radius-1) (neg? radius-2) (neg? height))
    (throw (ex-info "vol_conical_frustum() only accepts non-negative values"
                    {:type :ValueError})))
  (* (/ 1 3.0) Math/PI height
     (+ (Math/pow radius-1 2) (Math/pow radius-2 2) (* radius-1 radius-2))))

(defn vol-torus
  "Calculate the Volume of a Torus."
  [torus-radius tube-radius]
  (when (or (neg? torus-radius) (neg? tube-radius))
    (throw (ex-info "vol_torus() only accepts non-negative values"
                    {:type :ValueError})))
  (* 2 (Math/pow Math/PI 2) torus-radius (Math/pow tube-radius 2)))

(defn vol-octahedron
  "Calculate the Volume of an Octahedron."
  [oct-side]
  (when (neg? oct-side)
    (throw (ex-info "vol_octahedron() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* (Math/pow oct-side 3) (Math/sqrt 2)) 3))

(defn vol-icosahedron
  "Calculate the Volume of an Icosahedron."
  [tri-side]
  (when (neg? tri-side)
    (throw (ex-info "vol_icosahedron() only accepts non-negative values"
                    {:type :ValueError})))
  (/ (* (Math/pow tri-side 3) (+ 3 (Math/sqrt 5)) 5) 12))

(defn main
  "Print the Results of Various Volume Calculations."
  []
  (println "Volumes:")
  (println (str "Cube: vol_cube(2) = " (vol-cube 2)))
  (println (str "Cuboid: vol_cuboid(2, 2, 2) = " (vol-cuboid 2 2 2)))
  (println (str "Cone: vol_cone(2, 2) = " (vol-cone 2 2)))
  (println (str "Right Circular Cone: vol_right_circ_cone(2, 2) = "
                (vol-right-circ-cone 2 2)))
  (println (str "Prism: vol_prism(2, 2) = " (vol-prism 2 2)))
  (println (str "Pyramid: vol_pyramid(2, 2) = " (vol-pyramid 2 2)))
  (println (str "Sphere: vol_sphere(2) = " (vol-sphere 2)))
  (println (str "Hemisphere: vol_hemisphere(2) = " (vol-hemisphere 2)))
  (println (str "Circular Cylinder: vol_circular_cylinder(2, 2) = "
                (vol-circular-cylinder 2 2)))
  (println (str "Torus: vol_torus(2, 2) = " (vol-torus 2 2)))
  (println (str "Conical Frustum: vol_conical_frustum(2, 2, 4) = "
                (vol-conical-frustum 2 2 4)))
  (println (str "Spherical cap: vol_spherical_cap(1, 2) = "
                (vol-spherical-cap 1 2)))
  (println (str "Spheres intersection: vol_spheres_intersect(2, 2, 1) = "
                (vol-spheres-intersect 2 2 1)))
  (println (str "Spheres union: vol_spheres_union(2, 2, 1) = "
                (vol-spheres-union 2 2 1)))
  (println (str "Hollow Circular Cylinder: vol_hollow_circular_cylinder(1, 2, 3) = "
                (vol-hollow-circular-cylinder 1 2 3)))
  (println (str "Octahedron: vol_octahedron(2.5) = " (vol-octahedron 2.5)))
  (println (str "Icosahedron: vol_icosahedron(2.5) = " (vol-icosahedron 2.5)))
  nil)
