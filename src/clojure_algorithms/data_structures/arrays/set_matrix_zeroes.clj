(ns clojure-algorithms.data-structures.arrays.set-matrix-zeroes
  "Port of data_structures/arrays/set_matrix_zeroes.py.
  Returns a new matrix with zero rows/columns spread
  (the Python version mutates in place).")

(defn set-matrix-zeroes
  "New matrix where every row/column containing a 0 is all zeros."
  [matrix]
  (let [rows (count matrix)
        cols (if (pos? rows) (count (first matrix)) 0)
        zero-rows (set (for [i (range rows) :when (some zero? (nth matrix i))] i))
        zero-cols (set (for [j (range cols)
                             :when (some #(zero? (nth (nth matrix %) j))
                                         (range rows))]
                         j))]
    (mapv (fn [i row]
            (mapv (fn [j v]
                    (if (or (contains? zero-rows i) (contains? zero-cols j))
                      0 v))
                  (range) row))
          (range) matrix)))
