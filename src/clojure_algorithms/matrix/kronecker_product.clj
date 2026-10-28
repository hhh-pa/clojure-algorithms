(ns clojure-algorithms.matrix.kronecker-product
  "Kronecker product of two matrices.
  https://en.wikipedia.org/wiki/Kronecker_product")

(defn is-2d
  "True when matrix is a (possibly empty) sequence of sequences."
  [matrix]
  (boolean (and (sequential? matrix) (every? sequential? matrix))))

(defn kronecker-product
  "Returns the Kronecker product of matrix-a (m x n) and matrix-b (p x q)."
  [matrix-a matrix-b]
  (when-not (and (is-2d matrix-a) (is-2d matrix-b))
    (throw (ex-info "Input matrices must be 2-D." {:type :ValueError})))
  (if (or (empty? matrix-a) (empty? matrix-b))
    []
    (let [rows-a (count matrix-a) cols-a (count (first matrix-a))
          rows-b (count matrix-b) cols-b (count (first matrix-b))]
      (vec (for [ra (range rows-a) rb (range rows-b)]
             (vec (for [ca (range cols-a) cb (range cols-b)]
                    (* (get-in matrix-a [ra ca])
                       (get-in matrix-b [rb cb])))))))))
