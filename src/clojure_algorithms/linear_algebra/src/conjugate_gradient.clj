(ns clojure-algorithms.linear-algebra.src.conjugate-gradient
  "Conjugate-gradient solver for SPD systems A x = b (pure Clojure).
  SPD testing uses a Cholesky attempt instead of eigendecomposition.
  See https://en.wikipedia.org/wiki/Conjugate_gradient_method")

(defn- mat-vec [a v] (mapv (fn [row] (reduce + (map * row v))) a))
(defn- dot [a b] (reduce + (map * a b)))
(defn- norm [v] (Math/sqrt (reduce + (map #(* % %) v))))
(defn- vadd [a b] (mapv + a b))
(defn- vsub [a b] (mapv - a b))
(defn- vscale [v s] (mapv #(* % s) v))

(defn matrix-spd?
  "Returns true when square matrix is symmetric positive definite
  (symmetric with a successful Cholesky decomposition)."
  [matrix]
  (let [n (count matrix)]
    (and (= n (count (first matrix)))
         (= (mapv #(mapv double %) matrix)
            (apply mapv vector (mapv #(mapv double %) matrix)))
         (try
           (loop [i 0 l (vec (repeat n (vec (repeat n 0.0))))]
             (if (= i n)
               true
               (let [l (loop [j 0 l l]
                         (if (> j i)
                           l
                           (let [s (- (double (get-in matrix [i j]))
                                      (reduce + 0.0 (map (fn [k]
                                                           (* (get-in l [i k]) (get-in l [j k])))
                                                         (range j))))]
                             (if (= i j)
                               (do
                                 (when (<= s 0) (throw (ex-info "not spd" {})))
                                 (recur (inc j) (assoc-in l [i j] (Math/sqrt s))))
                               (recur (inc j) (assoc-in l [i j] (/ s (get-in l [j j]))))))))]
                 (recur (inc i) l))))
           (catch clojure.lang.ExceptionInfo _ false)))))

(defn conjugate-gradient
  "Solves spd-matrix * x = load-vector for SPD spd-matrix.
  Returns the solution vector."
  [spd-matrix load-vector & {:keys [max-iterations tol]
                             :or {max-iterations 1000 tol 1e-8}}]
  (assert (= (count spd-matrix) (count (first spd-matrix))) "matrix must be square")
  (assert (= (count spd-matrix) (count load-vector)) "dimension mismatch")
  (assert (matrix-spd? spd-matrix) "matrix must be symmetric positive definite")
  (let [n (count load-vector)
        b (mapv double load-vector)]
    (loop [x (vec (repeat n 0.0))
           r (vec b)
           p (vec b)
           iterations 0]
      (let [rs (dot r r)]
        (if (zero? rs)
          x
          (let [w (mat-vec spd-matrix p)
                alpha (/ rs (dot p w))
                x-new (vadd x (vscale p alpha))
                r-new (vsub r (vscale w alpha))
                beta (/ (dot r-new r-new) rs)
                p-new (vadd r-new (vscale p beta))
                error (max (norm (vsub r-new r)) (norm (vsub x-new x)))
                iterations (inc iterations)]
            (if (or (<= error tol) (> iterations max-iterations))
              x-new
              (recur x-new r-new p-new iterations))))))))
