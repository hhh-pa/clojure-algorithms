(ns clojure-algorithms.linear-algebra.src.lib
  "Vector and Matrix utilities over plain Clojure vectors (a port of the
  Python lib module's Vector/Matrix classes to idiomatic functions).
  Vectors are flat vectors; matrices are vectors of row vectors.")

;; ---- vectors ----

(defn zero-vector
  "Returns a zero vector of dimension."
  [dimension]
  (vec (repeat dimension 0)))

(defn unit-basis-vector
  "Returns the unit basis vector of dimension with 1 at pos."
  [dimension pos]
  (assoc (zero-vector dimension) pos 1))

(defn vadd
  "Adds two same-size vectors."
  [a b]
  (when (not= (count a) (count b))
    (throw (ex-info "must have the same size" {})))
  (mapv + a b))

(defn vsub
  "Subtracts vector b from vector a."
  [a b]
  (when (not= (count a) (count b))
    (throw (ex-info "must have the same size" {})))
  (mapv - a b))

(defn vscale
  "Multiplies vector v by scalar."
  [v scalar]
  (mapv #(* % scalar) v))

(defn dot
  "Returns the dot product of same-size vectors a and b."
  [a b]
  (when (not= (count a) (count b))
    (throw (ex-info "invalid operand!" {})))
  (reduce + (map * a b)))

(defn axpy
  "Computes scalar*x + y."
  [scalar x y]
  (vadd (vscale x scalar) y))

(defn random-vector
  "Returns a random integer vector of size n with components in [a,b]."
  [n a b]
  (mapv (fn [_] (+ a (rand-int (inc (- b a))))) (range n)))

(defn euclidean-length
  "Returns the Euclidean length of vector v."
  [v]
  (when (empty? v)
    (throw (ex-info "Vector is empty" {})))
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn angle
  "Returns the angle between vectors a and b (radians, or degrees
  when deg is true)."
  [a b & [deg]]
  (let [theta (Math/acos (/ (dot a b) (* (euclidean-length a) (euclidean-length b))))]
    (if deg (Math/toDegrees theta) theta)))

;; ---- matrices ----

(defn height [m] (count m))
(defn width [m] (count (first m)))

(defn component
  "Returns the [x y] component of matrix m."
  [m x y]
  (nth (nth m x) y))

(defn madd
  "Adds two same-dimension matrices."
  [a b]
  (when (or (not= (height a) (height b)) (not= (width a) (width b)))
    (throw (ex-info "matrix must have the same dimension!" {})))
  (mapv vadd a b))

(defn msub
  "Subtracts matrix b from matrix a."
  [a b]
  (when (or (not= (height a) (height b)) (not= (width a) (width b)))
    (throw (ex-info "matrices must have the same dimension!" {})))
  (mapv vsub a b))

(defn mscale
  "Multiplies matrix m by scalar."
  [m scalar]
  (mapv #(vscale % scalar) m))

(defn mat-vec
  "Multiplies matrix m by vector v."
  [m v]
  (when (not= (width m) (count v))
    (throw (ex-info "vector must have the same size as the number of columns of the matrix!" {})))
  (mapv #(dot % v) m))

(defn square-zero-matrix
  "Returns the NxN zero matrix."
  [n]
  (mapv (fn [_] (zero-vector n)) (range n)))

(defn random-matrix
  "Returns a random height x width integer matrix with components in [a,b]."
  [w h a b]
  (mapv (fn [_] (random-vector w a b)) (range h)))

(declare determinant)

(defn minor-matrix
  "Returns the minor value along (x, y) of square matrix m."
  [m x y]
  (when (not= (height m) (width m))
    (throw (ex-info "Matrix is not square" {})))
  (let [rows (vec (concat (subvec m 0 x) (subvec m (inc x))))]
    (determinant (mapv #(vec (concat (subvec % 0 y) (subvec % (inc y)))) rows))))

(defn cofactor
  "Returns the cofactor (signed minor) along (x, y)."
  [m x y]
  (when (not= (height m) (width m))
    (throw (ex-info "Matrix is not square" {})))
  (* (Math/pow -1 (+ x y)) (minor-matrix m x y)))

(defn determinant
  "Returns the determinant of square matrix m via Laplace expansion."
  [m]
  (when (not= (height m) (width m))
    (throw (ex-info "Matrix is not square" {})))
  (when (< (height m) 1)
    (throw (ex-info "Matrix has no element" {})))
  (case (height m)
    1 (get-in m [0 0])
    2 (- (* (get-in m [0 0]) (get-in m [1 1]))
         (* (get-in m [0 1]) (get-in m [1 0])))
    (reduce + (map-indexed (fn [y v] (* v (cofactor m 0 y))) (first m)))))
