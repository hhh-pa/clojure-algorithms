(ns clojure-algorithms.linear-algebra.gauss-jordan
  "Gauss-Jordan elimination to Reduced Row Echelon Form.
  See https://en.wikibooks.org/wiki/Linear_Algebra/Gauss-Jordan_Reduction")

(defn- close-to-zero? [x] (< (Math/abs (double x)) 1e-9))

(defn gauss-jordan
  "Reduces the system A*x = b to RREF. A is a nested vector matrix,
  b a flat vector. Returns [rref-A transformed-b]. Throws when the
  row counts of A and b differ."
  [coefficients vertices]
  (let [a (mapv #(mapv double %) coefficients)
        b (mapv double vertices)
        rows (count a)
        cols (count (first a))]
    (when (not= rows (count b))
      (throw (ex-info "Number of rows in coefficients and vertices must match." {})))
    (loop [col 0 a a b b]
      (if (>= col cols)
        [a b]
        (let [pivot-row (first (filter #(not (close-to-zero? (get-in a [% col])))
                                       (range col rows)))]
          (if (nil? pivot-row)
            (recur (inc col) a b)
            (let [a (if (= pivot-row col) a
                        (-> a (assoc col (nth a pivot-row)) (assoc pivot-row (nth a col))))
                  b (if (= pivot-row col) b
                        (-> b (assoc col (nth b pivot-row)) (assoc pivot-row (nth b col))))
                  pivot (get-in a [col col])
                  a (assoc a col (mapv #(/ % pivot) (nth a col)))
                  b (assoc b col (/ (nth b col) pivot))
                  [a b] (reduce (fn [[a b] row]
                                  (if (= row col)
                                    [a b]
                                    (let [factor (get-in a [row col])]
                                      [(assoc a row (mapv - (nth a row)
                                                          (mapv #(* factor %) (nth a col))))
                                       (assoc b row (- (nth b row) (* factor (nth b col))))])))
                                [a b]
                                (range rows))]
              (recur (inc col) a b))))))))
