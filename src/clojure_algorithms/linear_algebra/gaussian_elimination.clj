(ns clojure-algorithms.linear-algebra.gaussian-elimination
  "Gaussian elimination for systems of linear equations.
  See https://en.wikipedia.org/wiki/Gaussian_elimination")

(defn retroactive-resolution
  "Solves an upper-triangular system coefficients*x = vector by back
  substitution. Returns x as a vector. Matrices are vectors of vectors."
  [coefficients vector]
  (let [rows (count coefficients)]
    (loop [row (dec rows)
           x (vec (repeat rows 0.0))]
      (if (neg? row)
        x
        (let [total (reduce + 0.0 (map * (subvec (nth coefficients row) (inc row))
                                       (subvec x (inc row))))]
          (recur (dec row)
                 (assoc x row (/ (- (nth vector row) total)
                                 (nth (nth coefficients row) row)))))))))

(defn gaussian-elimination
  "Solves coefficients*x = vector via Gaussian elimination.
  Returns [] when coefficients is not square."
  [coefficients vector]
  (let [rows (count coefficients)
        cols (count (first coefficients))]
    (if (not= rows cols)
      []
      (let [aug (mapv (fn [row v] (conj (mapv double row) (double v)))
                      coefficients vector)
            aug (loop [r 0 aug aug]
                  (if (>= r (dec rows))
                    aug
                    (let [pivot (get-in aug [r r])]
                      (recur (inc r)
                             (reduce (fn [a c]
                                       (let [factor (/ (get-in a [c r]) pivot)]
                                         (assoc a c (mapv - (nth a c)
                                                          (mapv #(* factor %) (nth a r))))))
                                     aug
                                     (range (inc r) rows))))))
            a (mapv #(subvec % 0 cols) aug)
            v (mapv #(nth % cols) aug)]
        (retroactive-resolution a v)))))
