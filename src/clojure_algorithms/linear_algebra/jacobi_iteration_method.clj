(ns clojure-algorithms.linear-algebra.jacobi-iteration-method
  "Jacobi iterative solver for strictly diagonally dominant systems.
  See https://en.wikipedia.org/wiki/Jacobi_method")

(defn strictly-diagonally-dominant?
  "Returns true when the augmented table's coefficient part is strictly
  diagonally dominant; throws otherwise."
  [table]
  (let [rows (count table)
        cols (count (first table))]
    (doseq [i (range rows)]
      (let [total (reduce + (map (nth table i)
                                 (remove #{i} (range (dec cols)))))]
        (when (<= (get-in table [i i]) total)
          (throw (ex-info "Coefficient matrix is not strictly diagonally dominant" {})))))
    true))

(defn jacobi-iteration-method
  "Solves coefficient-matrix*x = constant-vector by Jacobi iteration,
  starting from init-val for iterations steps. The constant is a flat
  vector. Throws on dimension mismatches or non-positive iterations."
  [coefficient-matrix constant-vector init-val iterations]
  (let [rows1 (count coefficient-matrix)
        cols1 (count (first coefficient-matrix))
        rows2 (count constant-vector)]
    (when (not= rows1 cols1)
      (throw (ex-info (format "Coefficient matrix dimensions must be nxn but received %dx%d"
                              rows1 cols1) {})))
    (when (not= rows1 rows2)
      (throw (ex-info (format (str "Coefficient and constant matrices dimensions must be "
                                   "nxn and nx1 but received %dx%d and %dx1")
                              rows1 cols1 rows2) {})))
    (when (not= (count init-val) rows1)
      (throw (ex-info (format (str "Number of initial values must be equal to number of "
                                   "rows in coefficient matrix but received %d and %d")
                              (count init-val) rows1) {})))
    (when (<= iterations 0)
      (throw (ex-info "Iterations must be at least 1" {})))
    (strictly-diagonally-dominant?
     (mapv (fn [row c] (conj (vec row) c)) coefficient-matrix constant-vector))
    (loop [n iterations
           current (mapv double init-val)]
      (if (zero? n)
        (vec current)
        (recur (dec n)
               (mapv (fn [row c i]
                       (/ (+ c (reduce + (map-indexed
                                          (fn [j aij]
                                            (if (= i j) 0.0 (* -1.0 aij (nth current j))))
                                          row)))
                          (nth row i)))
                     coefficient-matrix constant-vector (range rows1)))))))
