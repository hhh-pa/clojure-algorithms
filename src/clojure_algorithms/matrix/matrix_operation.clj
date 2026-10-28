(ns clojure-algorithms.matrix.matrix-operation
  "Functions for 2D matrix operations.")

(defn -check-not-integer
  "True when matrix looks like a matrix rather than an int."
  [matrix]
  (and (not (integer? matrix)) (not (integer? (first matrix)))))

(defn -shape
  "Returns [rows cols] of the matrix."
  [matrix]
  [(count matrix) (count (first matrix))])

(defn -verify-matrix-sizes
  "Throws unless the two matrices have mutually transposed shapes.
  Returns [[rows-a rows-b] [cols-a cols-b]]."
  [matrix-a matrix-b]
  (let [[r1 c1] (-shape matrix-a)
        [r2 c2] (-shape matrix-b)]
    (when (or (not= r1 c2) (not= c1 r2))
      (throw (ex-info (str "operands could not be broadcast together with shape "
                           "(" [r1 c1] "), (" [r2 c2] ")")
                      {:type :ValueError})))
    [[r1 r2] [c1 c2]]))

(defn add
  "Adds any number of matrices element by element."
  [& matrix-s]
  (if (every? -check-not-integer matrix-s)
    (do
      (doseq [m (rest matrix-s)]
        (-verify-matrix-sizes (first matrix-s) m))
      (reduce (fn [acc m]
                (mapv (fn [ra rb] (mapv +' ra rb)) acc m))
              matrix-s))
    (throw (ex-info "Expected a matrix, got int/list instead"
                    {:type :TypeError}))))

(defn subtract
  "Subtracts matrix-b from matrix-a element by element."
  [matrix-a matrix-b]
  (if (and (-check-not-integer matrix-a)
           (-check-not-integer matrix-b)
           (do (-verify-matrix-sizes matrix-a matrix-b) true))
    (mapv (fn [ra rb] (mapv -' ra rb)) matrix-a matrix-b)
    (throw (ex-info "Expected a matrix, got int/list instead"
                    {:type :TypeError}))))

(defn scalar-multiply
  "Multiplies every element of matrix by n."
  [matrix n]
  (mapv (fn [row] (mapv (fn [x] (* x n)) row)) matrix))

(defn multiply
  "Multiplies matrix-a by matrix-b."
  [matrix-a matrix-b]
  (if (and (-check-not-integer matrix-a) (-check-not-integer matrix-b))
    (let [[[r1 r2] [c1 c2]] (-verify-matrix-sizes matrix-a matrix-b)]
      (when (not= c1 r2)
        (throw (ex-info (str "Cannot multiply matrix of dimensions ("
                             r1 "," c1 ") and (" r2 "," c2 ")")
                        {:type :ValueError})))
      (let [cols (apply map vector matrix-b)]
        (mapv (fn [row]
                (mapv (fn [col] (reduce +' 0 (map *' row col))) cols))
              matrix-a)))
    (throw (ex-info "Expected a matrix, got int/list instead"
                    {:type :TypeError}))))

(defn identity
  "Returns the n x n identity matrix."
  [n]
  (let [n (long n)]
    (mapv (fn [r] (mapv (fn [c] (if (= r c) 1 0)) (range n))) (range n))))

(defn transpose
  "Transposes the matrix. With return-map true (the default) returns a
  lazy sequence of rows, mirroring Python's map object."
  ([matrix] (transpose matrix true))
  ([matrix return-map]
   (if (-check-not-integer matrix)
     (let [t (apply map vector matrix)]
       (if return-map
         (map vec t)
         (mapv vec t)))
     (throw (ex-info "Expected a matrix, got int/list instead"
                     {:type :TypeError})))))

(defn minor
  "Returns the matrix with row and column removed."
  [matrix row column]
  (let [rows (vec matrix)
        kept (vec (concat (subvec rows 0 row) (subvec rows (inc row))))]
    (mapv (fn [r]
            (let [r (vec r)]
              (vec (concat (subvec r 0 column) (subvec r (inc column))))))
          kept)))

(defn determinant
  "Returns the determinant via cofactor expansion along the first row."
  [matrix]
  (if (= 1 (count matrix))
    (get-in matrix [0 0])
    (reduce +'
            0
            (map-indexed (fn [i x]
                           (*' x
                               (determinant (minor matrix 0 i))
                               (if (odd? i) -1 1)))
                         (first matrix)))))

(defn permanent
  "Returns the permanent, like the determinant but all signs positive."
  [matrix]
  (if (= 1 (count matrix))
    (get-in matrix [0 0])
    (reduce +'
            0
            (map-indexed (fn [i x] (*' x (permanent (minor matrix 0 i))))
                         (first matrix)))))

(defn inverse
  "Returns the inverse matrix, or nil when the matrix is singular."
  [matrix]
  (let [det (determinant matrix)]
    (when-not (zero? det)
      (let [n (count matrix)
            minors (mapv (fn [i]
                           (mapv (fn [j] (determinant (minor matrix i j)))
                                 (range n)))
                         (range n))
            cofactors (mapv (fn [i row]
                              (mapv (fn [j x]
                                      (*' x (if (odd? (+ i j)) -1 1)))
                                    (range)
                                    row))
                            (range)
                            minors)
            adjugate (transpose cofactors false)]
        (scalar-multiply adjugate (/ 1.0 det))))))

(defn main
  "Demonstrates the matrix operations."
  []
  (let [matrix-a [[12 10] [3 9]]
        matrix-b [[3 4] [7 4]]
        matrix-c [[11 12 13 14] [21 22 23 24] [31 32 33 34] [41 42 43 44]]
        matrix-d [[3 0 2] [2 0 -2] [0 1 1]]]
    (println (str "Add Operation, add(matrix_a, matrix_b) = "
                  (pr-str (add matrix-a matrix-b)) " \n"))
    (println (str "Multiply Operation, multiply(matrix_a, matrix_b) = "
                  (pr-str (multiply matrix-a matrix-b)) " \n"))
    (println (str "Identity: " (pr-str (identity 5)) "\n"))
    (println (str "Minor of " (pr-str matrix-c) " = "
                  (pr-str (minor matrix-c 1 2)) " \n"))
    (println (str "Determinant of " (pr-str matrix-b) " = "
                  (determinant matrix-b) " \n"))
    (println (str "Permanent of " (pr-str matrix-b) " = "
                  (permanent matrix-b) " \n"))
    (println (str "Inverse of " (pr-str matrix-d) " = "
                  (pr-str (inverse matrix-d)) "\n"))))
