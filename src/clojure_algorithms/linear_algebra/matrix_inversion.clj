(ns clojure-algorithms.linear-algebra.matrix-inversion
  "Square-matrix inversion via Gauss-Jordan elimination (pure Clojure).")

(defn invert-matrix
  "Returns the inverse of square matrix as nested vectors.
  Throws when the matrix is not invertible."
  [matrix]
  (let [n (count matrix)
        aug (mapv (fn [row i]
                    (vec (concat (mapv double row)
                                 (mapv #(if (= % i) 1.0 0.0) (range n)))))
                  matrix (range n))]
    (loop [col 0 aug aug]
      (if (= col n)
        (mapv #(subvec % n) aug)
        (let [pivot (first (filter #(> (Math/abs (double (get-in aug [% col]))) 1e-12)
                                   (range col n)))]
          (when (nil? pivot)
            (throw (ex-info "Matrix is not invertible" {})))
          (let [aug (if (= pivot col) aug
                        (-> aug (assoc col (nth aug pivot)) (assoc pivot (nth aug col))))
                pv (get-in aug [col col])
                aug (assoc aug col (mapv #(/ % pv) (nth aug col)))
                aug (reduce (fn [aug row]
                              (if (= row col)
                                aug
                                (let [factor (get-in aug [row col])]
                                  (assoc aug row (mapv - (nth aug row)
                                                       (mapv #(* factor %) (nth aug col)))))))
                            aug (range n))]
            (recur (inc col) aug)))))))
