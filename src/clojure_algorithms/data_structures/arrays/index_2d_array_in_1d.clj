(ns clojure-algorithms.data-structures.arrays.index-2d-array-in-1d
  "Port of data_structures/arrays/index_2d_array_in_1d.py.
  Treats a 2D array as a flat 1D sequence.")

(defn flatten-2d
  "Lazy seq of MATRIX rows concatenated."
  [matrix]
  (apply concat matrix))

(defn index-2d-array-in-1d
  "Element of rectangular MATRIX at flat 1D INDEX."
  [matrix index]
  (let [rows (count matrix)
        cols (if (pos? rows) (count (first matrix)) 0)]
    (when (or (zero? rows) (zero? cols))
      (throw (ex-info "no items in array" {:type :value-error})))
    (when-not (<= 0 index (dec (* rows cols)))
      (throw (ex-info "index out of range" {:type :value-error})))
    (nth (nth matrix (quot index cols)) (rem index cols))))
