(ns clojure-algorithms.matrix.cramers-rule-2x2
  "Solves a 2x2 system of linear equations with Cramer's rule.")

(defn cramers-rule-2x2
  "Solves the system given as [a1 b1 d1] and [a2 b2 d2].
  Returns [x y] as doubles."
  [equation1 equation2]
  (when-not (= 3 (count equation1) (count equation2))
    (throw (ex-info "Please enter a valid equation." {:type :ValueError})))
  (when (and (zero? (nth equation1 0)) (zero? (nth equation1 1))
             (zero? (nth equation2 0)) (zero? (nth equation2 1)))
    (throw (ex-info "Both a & b of two equations can't be zero."
                    {:type :ValueError})))
  (let [[a1 b1 c1] equation1
        [a2 b2 c2] equation2
        determinant (- (* a1 b2) (* a2 b1))
        determinant-x (- (* c1 b2) (* c2 b1))
        determinant-y (- (* a1 c2) (* a2 c1))]
    (cond (and (zero? determinant) (zero? determinant-x) (zero? determinant-y))
          (throw (ex-info "Infinite solutions. (Consistent system)"
                          {:type :ValueError}))
          (zero? determinant)
          (throw (ex-info "No solution. (Inconsistent system)"
                          {:type :ValueError}))
          (and (zero? determinant-x) (zero? determinant-y))
          [0.0 0.0]
          :else
          [(double (/ determinant-x determinant))
           (double (/ determinant-y determinant))])))
