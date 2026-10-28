(ns clojure-algorithms.geometry.geometry
  "2D shapes: angles, sides, ellipses, circles and polygons.")

(defrecord Angle [degrees])
(defrecord Side [length angle next-side])
(defrecord Ellipse [major-radius minor-radius])
(defrecord Polygon [sides])

(defn make-angle
  "Builds an Angle in degrees (0-360)."
  ([] (->Angle 90))
  ([degrees]
   (when-not (and (number? degrees) (<= 0 degrees 360))
     (throw (IllegalArgumentException.
             "degrees must be a numeric value between 0 and 360.")))
   (->Angle degrees)))

(defn angle-to-radians [angle] (Math/toRadians (:degrees angle)))

(defn angle-from-radians
  "Builds an Angle from radians, normalized to 0-360."
  [radians] (make-angle (mod (Math/toDegrees radians) 360)))

(defn make-side
  "Builds a Side with positive length, an Angle and optional next Side."
  ([length] (make-side length (make-angle) nil))
  ([length angle] (make-side length angle nil))
  ([length angle next-side]
   (when-not (and (number? length) (pos? length))
     (throw (IllegalArgumentException. "length must be a positive numeric value.")))
   (when-not (instance? Angle angle)
     (throw (IllegalArgumentException. "angle must be an Angle object.")))
   (when-not (or (nil? next-side) (instance? Side next-side))
     (throw (IllegalArgumentException. "next_side must be a Side or None.")))
   (->Side length angle next-side)))

(defn ellipse-area [{:keys [major-radius minor-radius]}]
  (* Math/PI major-radius minor-radius))

(defn ellipse-perimeter
  "Ramanujan's first approximation of the ellipse perimeter."
  [{:keys [major-radius minor-radius]}]
  (let [a major-radius b minor-radius]
    (* Math/PI (- (* 3 (+ a b)) (Math/sqrt (* (+ (* 3 a) b) (+ a (* 3 b))))))))

(defn ellipse-eccentricity [{:keys [major-radius minor-radius]}]
  (let [a (max major-radius minor-radius) b (min major-radius minor-radius)]
    (Math/sqrt (- 1 (* (/ b a) (/ b a))))))

(defn make-circle
  "Builds a circle (ellipse with equal radii) plus its radius."
  [radius] {:ellipse (->Ellipse radius radius) :radius radius})

(defn circle-diameter [circle] (* (:radius circle) 2))

(defn max-parts
  "Maximum pieces of a circle after num-cuts cuts."
  [_circle num-cuts]
  (when-not (and (number? num-cuts) (>= num-cuts 0))
    (throw (IllegalArgumentException. "num_cuts must be a positive numeric value.")))
  (* (+ num-cuts 2 (* num-cuts num-cuts)) 0.5))

(defn make-polygon [] (->Polygon []))

(defn add-side
  "Appends side, linking the previous last side to it."
  [polygon side]
  (let [sides (if (seq (:sides polygon))
                (assoc (:sides polygon) (dec (count (:sides polygon)))
                       (assoc (peek (:sides polygon)) :next-side side))
                [])]
    (->Polygon (conj sides side))))

(defn get-side [polygon index] (nth (:sides polygon) index))

(defn set-side [polygon index side]
  (->Polygon (assoc (:sides polygon) index side)))

(defn get-vertices
  "Walks the polygon sides from the origin; throws when it does not close."
  [polygon]
  (if (empty? (:sides polygon))
    []
    (let [vertices (reduce (fn [{:keys [x y dir acc]} side]
                             (let [nx (+ x (* (:length side) (Math/cos dir)))
                                   ny (+ y (* (:length side) (Math/sin dir)))]
                               {:x nx :y ny :dir (+ dir (- Math/PI (angle-to-radians (:angle side))))
                                :acc (conj acc [nx ny])}))
                           {:x 0.0 :y 0.0 :dir 0.0 :acc [[0.0 0.0]]}
                           (:sides polygon))
          vs (:acc vertices)
          [lx ly] (peek vs)]
      (when (> (Math/hypot lx ly) 1e-6)
        (throw (IllegalArgumentException.
                "Polygon does not close back to starting point")))
      vs)))

(defn polygon-perimeter [polygon]
  (reduce + 0 (map :length (:sides polygon))))

(defn polygon-area
  "Absolute shoelace area of the polygon."
  [polygon]
  (let [vs (get-vertices polygon)]
    (if (< (count vs) 3)
      0.0
      (let [n (count vs)]
        (/ (Math/abs (double (reduce + (map-indexed
                                        (fn [i [x1 y1]]
                                          (let [[x2 y2] (nth vs (mod (inc i) n))]
                                            (- (* x1 y2) (* x2 y1))))
                                        vs))))
           2.0)))))

(defn make-rectangle
  "Builds a rectangle polygon from short and long side lengths."
  [short-side-length long-side-length]
  (-> (make-polygon)
      (add-side (make-side short-side-length))
      (add-side (make-side long-side-length))
      (add-side (make-side short-side-length))
      (add-side (make-side long-side-length))))

(defn rectangle-perimeter [rect] (polygon-perimeter rect))
(defn rectangle-area [rect] (polygon-area rect))
(defn make-square [side-length] (make-rectangle side-length side-length))

(defn make-triangle
  "Builds a triangle polygon; validates the triangle inequality and that
   angles sum to 180 degrees."
  ([a b c] (make-triangle a b c nil nil nil))
  ([a b c angle-a angle-b angle-c]
   (when-not (and (> (+ a b) c) (> (+ a c) b) (> (+ b c) a))
     (throw (IllegalArgumentException. "Sides must satisfy triangle inequality")))
   (let [clamp #(max -1 (min 1 %))
         ang (fn [adj1 adj2 opp]
               (angle-from-radians
                (Math/acos (clamp (/ (- (+ (* adj1 adj1) (* adj2 adj2)) (* opp opp))
                                     (* 2 adj1 adj2))))))
         aa (or angle-a (ang b c a))
         ab (or angle-b (ang a c b))
         ac (or angle-c (ang a b c))]
     (when (> (Math/abs (- (+ (:degrees aa) (:degrees ab) (:degrees ac)) 180)) 0.01)
       (throw (IllegalArgumentException. "Triangle angles must sum to 180 degrees")))
     {:sides [a b c] :angles [aa ab ac]
      :polygon (-> (make-polygon)
                   (add-side (make-side a aa))
                   (add-side (make-side b ab))
                   (add-side (make-side c ac)))})))

(defn triangle-perimeter [tri] (reduce + (:sides tri)))

(defn triangle-area
  "Heron's formula."
  [tri]
  (let [[a b c] (:sides tri)
        s (/ (triangle-perimeter tri) 2.0)]
    (Math/sqrt (* s (- s a) (- s b) (- s c)))))
