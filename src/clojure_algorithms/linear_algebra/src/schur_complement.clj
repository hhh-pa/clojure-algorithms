(ns clojure-algorithms.linear-algebra.src.schur-complement
  "Schur complement S = C - B^T A^-1 B of a 2x2 block matrix.
  See https://en.wikipedia.org/wiki/Schur_complement")

(defn- mat-mul [a b]
  (let [bt (apply mapv vector b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn- mat-sub [a b] (mapv (fn [ra rb] (mapv - ra rb)) a b))

(defn- transpose [m] (apply mapv vector m))

(defn- inverse
  "Gauss-Jordan inverse of square matrix m; throws when singular."
  [m]
  (let [n (count m)
        aug (mapv (fn [row i]
                    (vec (concat (mapv double row)
                                 (mapv #(if (= % i) 1.0 0.0) (range n)))))
                  m (range n))]
    (loop [col 0 aug aug]
      (if (= col n)
        (mapv #(subvec % n) aug)
        (let [pivot (first (filter #(> (Math/abs (double (get-in aug [% col]))) 1e-12)
                                   (range col n)))]
          (when (nil? pivot)
            (throw (ex-info "Input matrix A is not invertible. Cannot compute Schur complement." {})))
          (let [aug (if (= pivot col) aug
                        (-> aug (assoc col (nth aug pivot)) (assoc pivot (nth aug col))))
                pv (get-in aug [col col])
                aug (assoc aug col (mapv #(/ % pv) (nth aug col)))
                aug (reduce (fn [aug row]
                              (if (= row col)
                                aug
                                (let [f (get-in aug [row col])]
                                  (assoc aug row (mapv - (nth aug row)
                                                       (mapv #(* f %) (nth aug col)))))))
                            aug (range n))]
            (recur (inc col) aug)))))))

(defn schur-complement
  "Returns C - B^T A^-1 B. Throws when A and B row counts or B and C
  column counts mismatch, or A is singular."
  [mat-a mat-b mat-c]
  (when (not= (count mat-a) (count mat-b))
    (throw (ex-info (format "Expected the same number of rows for A and B. Instead found A of size %s and B of size %s"
                            [(count mat-a) (count (first mat-a))]
                            [(count mat-b) (count (first mat-b))]) {})))
  (when (not= (count (first mat-b)) (count (first mat-c)))
    (throw (ex-info (format "Expected the same number of columns for B and C. Instead found B of size %s and C of size %s"
                            [(count mat-b) (count (first mat-b))]
                            [(count mat-c) (count (first mat-c))]) {})))
  (mat-sub (mapv #(mapv double %) mat-c)
           (mat-mul (mat-mul (transpose (mapv #(mapv double %) mat-b))
                             (inverse (mapv #(mapv double %) mat-a)))
                    (mapv #(mapv double %) mat-b))))
