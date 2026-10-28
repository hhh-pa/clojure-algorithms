(ns clojure-algorithms.linear-algebra.src.gaussian-elimination-pivoting
  "Gaussian elimination with partial pivoting.
  Reference: CS 357 lecture 7, UIUC.")

(defn solve-linear-system
  "Solves the linear system given as an augmented matrix (last column
  holds the constants). Returns the solution vector. Throws when the
  matrix is not square or is singular."
  [matrix]
  (let [ab (mapv #(mapv double %) matrix)
        nrows (count ab)
        ncols (dec (count (first ab)))]
    (when (not= nrows ncols)
      (throw (ex-info "Matrix is not square" {})))
    (let [ab (loop [col 0 ab ab]
               (if (>= col nrows)
                 ab
                 (let [best (apply max-key #(Math/abs (double (get-in ab [% col]))) (range col ncols))
                       ab (if (= best col) ab
                              (-> ab (assoc col (nth ab best)) (assoc best (nth ab col))))]
                   (when (< (Math/abs (double (get-in ab [col col]))) 1e-8)
                     (throw (ex-info "Matrix is singular" {})))
                   (recur (inc col)
                          (if (zero? col)
                            ab
                            (reduce (fn [ab i]
                                      (let [factor (/ (get-in ab [i (dec col)])
                                                      (get-in ab [(dec col) (dec col)]))]
                                        (assoc ab i (mapv - (nth ab i)
                                                          (mapv #(* factor %) (nth ab (dec col)))))))
                                    ab (range col nrows)))))))]
      (loop [col (dec nrows)
             ab ab
             xs []]
        (if (neg? col)
          (vec xs)
          (let [x (/ (nth (nth ab col) ncols) (get-in ab [col col]))
                ab (reduce (fn [ab i]
                             (assoc ab i (assoc (nth ab i) ncols
                                                (- (nth (nth ab i) ncols)
                                                   (* (get-in ab [i col]) x)))))
                           ab (range (dec col) -1 -1))]
            (recur (dec col) ab (cons x xs))))))))
