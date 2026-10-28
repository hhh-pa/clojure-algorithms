(ns clojure-algorithms.linear-algebra.src.rank-of-matrix
  "Rank of a matrix via Gaussian elimination.
  See https://en.wikipedia.org/wiki/Rank_(linear_algebra)")

(defn rank-of-matrix
  "Returns the rank of matrix (a vector of row vectors). Does not
  mutate its input."
  [matrix]
  (let [rows (count matrix)
        columns (count (first matrix))]
    (loop [row 0
           rank (min rows columns)
           mat (mapv #(mapv double %) matrix)]
      (if (>= row rank)
        rank
        (if (not (zero? (get-in mat [row row])))
          (recur (inc row) rank
                 (reduce (fn [mat col]
                           (let [multiplier (/ (get-in mat [col row])
                                               (get-in mat [row row]))]
                             (assoc mat col
                                    (vec (map-indexed (fn [i v]
                                                        (if (>= i row)
                                                          (- v (* multiplier (get-in mat [row i])))
                                                          v))
                                                      (nth mat col))))))
                         mat (range (inc row) rows)))
          (let [swap (first (filter #(not (zero? (get-in mat [% row])))
                                    (range (inc row) rows)))]
            (if swap
              (recur row rank
                     (-> mat
                         (assoc row (nth mat swap))
                         (assoc swap (nth mat row))))
              (let [rank (dec rank)
                    mat (mapv (fn [r] (assoc r row (nth r rank))) mat)]
                (recur row rank mat)))))))))
