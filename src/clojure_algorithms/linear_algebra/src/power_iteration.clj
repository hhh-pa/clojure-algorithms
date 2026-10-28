(ns clojure-algorithms.linear-algebra.src.power-iteration
  "Power iteration for the dominant eigenpair of a real symmetric matrix.
  (The Python original also supports complex Hermitian input; this port
  covers the real case with no external dependencies.)")

(defn- mat-vec [a v] (mapv (fn [row] (reduce + (map * row v))) a))
(defn- norm [v] (Math/sqrt (reduce + (map #(* % %) v))))
(defn- dot [a b] (reduce + (map * a b)))

(defn power-iteration
  "Returns [largest-eigenvalue largest-eigenvector] for square matrix
  a starting from vector. Stops when the eigenvalue changes by less
  than error-tol or max-iterations is reached."
  [a vector & {:keys [error-tol max-iterations]
               :or {error-tol 1e-12 max-iterations 100}}]
  (assert (= (count a) (count (first a))) "matrix must be square")
  (assert (= (count a) (count vector)) "dimension mismatch")
  (loop [v (mapv double vector)
         lambda-prev 0.0
         iter 0]
    (let [w (mat-vec a v)
          v (mapv #(/ % (norm w)) w)
          lambda (dot v (mat-vec a v))
          err (/ (Math/abs (- lambda lambda-prev)) (Math/abs lambda))
          iter (inc iter)]
      (if (or (<= err error-tol) (>= iter max-iterations))
        [lambda v]
        (recur v lambda iter)))))
