(ns clojure-algorithms.maths.ear-clipping-polygon-triangulation
  "An implementation of the ear clipping method for triangulating a simple polygon.

Wikipedia : https://en.wikipedia.org/wiki/Polygon_triangulation")

(defn cross-product
  "Compute the cross product of two vectors given three points.
  Zero when collinear, positive for counter-clockwise turns, negative for
  clockwise turns."
  [p1 p2 p3]
  (- (* (- (nth p2 0) (nth p1 0)) (- (nth p3 1) (nth p1 1)))
     (* (- (nth p2 1) (nth p1 1)) (- (nth p3 0) (nth p1 0)))))

(defn is-convex
  "Determine whether three points form a convex angle given the polygon
  orientation (\"clockwise\" or \"counter-clockwise\")."
  [point prev-p next-p direction]
  (let [[px py] point
        [ax ay] prev-p
        [bx by] next-p]
    (if (= direction "counter-clockwise")
      (>= (- (* (- bx px) (- ay py)) (* (- ax px) (- by py))) 0)
      (>= (- (* (- ax px) (- by py)) (* (- bx px) (- ay py))) 0))))

(defn- triangle-area
  "Absolute area of the triangle with vertices a, b, c."
  [a b c]
  (Math/abs (* 0.5 (+ (* (nth a 0) (- (nth b 1) (nth c 1)))
                      (* (nth b 0) (- (nth c 1) (nth a 1)))
                      (* (nth c 0) (- (nth a 1) (nth b 1)))))))

(defn is-point-inside-triangle
  "Determine whether test-point lies inside (or on the edge of) the triangle
  formed by p1, p2 and p3, by comparing the triangle area with the sum of
  the three sub-triangle areas."
  [p1 p2 p3 test-point]
  (== (triangle-area p1 p2 p3)
      (+ (triangle-area test-point p2 p3)
         (triangle-area p1 test-point p3)
         (triangle-area p1 p2 test-point))))

(defn is-ear
  "Determine whether the vertex at point-idx forms an ear: it must be
  convex and the triangle it forms with its neighbours must contain no
  other polygon vertex."
  [polygon point-idx direction]
  (let [n (count polygon)
        prev-idx (mod (dec point-idx) n)
        next-idx (mod (inc point-idx) n)
        prev-point (nth polygon prev-idx)
        point (nth polygon point-idx)
        next-point (nth polygon next-idx)]
    (boolean
     (and (is-convex prev-point point next-point direction)
          (not-any? (fn [j]
                      (and (not (contains? #{prev-idx point-idx next-idx} j))
                           (is-point-inside-triangle prev-point point next-point
                                                     (nth polygon j))))
                    (range n))))))

(defn direction
  "Determine the orientation (\"clockwise\" or \"counter-clockwise\") of a
  polygon given as a sequential of points."
  [polygon]
  (let [polygon (vec polygon)
        point-0 (reduce (fn [a b]
                          (if (neg? (compare [(nth b 1) (nth b 0)]
                                             [(nth a 1) (nth a 0)]))
                            b
                            a))
                        polygon)
        idx (.indexOf polygon point-0)
        n (count polygon)
        prev-point (nth polygon (mod (dec idx) n))
        next-point (loop [ni (mod (inc idx) n)]
                     (let [np (nth polygon ni)]
                       (if (zero? (cross-product point-0 np prev-point))
                         (recur (inc ni))
                         np)))]
    (if (pos? (cross-product point-0 next-point prev-point))
      "clockwise"
      "counter-clockwise")))

(defn triangulate-polygon
  "Triangulate a simple polygon given as a sequential of [x y] vertices.
  Returns a vector of triangles, each a vector of three vertices."
  [coordinates]
  (let [orientation (direction coordinates)]
    (loop [polygon (vec coordinates)
           triangles []]
      (if (< (count polygon) 3)
        triangles
        (if-let [i (first (filter #(is-ear polygon % orientation)
                                  (range (count polygon))))]
          (let [n (count polygon)
                prev-point (nth polygon (mod (dec i) n))
                point (nth polygon i)
                next-point (nth polygon (mod (inc i) n))]
            (recur (vec (concat (subvec polygon 0 i)
                                (subvec polygon (inc i))))
                   (conj triangles [prev-point point next-point])))
          triangles)))))
