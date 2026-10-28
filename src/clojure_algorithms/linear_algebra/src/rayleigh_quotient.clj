(ns clojure-algorithms.linear-algebra.src.rayleigh-quotient
  "Rayleigh quotient of a real symmetric matrix and a vector.
  See https://en.wikipedia.org/wiki/Rayleigh_quotient")

(defn- transpose [m] (apply mapv vector m))

(defn hermitian?
  "Returns true when real matrix is symmetric (the real case of
  Hermitian)."
  [matrix]
  (= (mapv #(mapv double %) matrix)
     (transpose (mapv #(mapv double %) matrix))))

(defn rayleigh-quotient
  "Returns (v^T A v) / (v^T v) for matrix a and vector v."
  [a v]
  (let [v (mapv double v)
        av (mapv (fn [row] (reduce + (map * row v))) a)]
    (/ (reduce + (map * v av))
       (reduce + (map * v v)))))
