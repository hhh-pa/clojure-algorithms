(ns clojure-algorithms.matrix.binary-search-matrix
  "Binary search over a sorted 1D array, applied row by row to a 2D matrix.")

(defn binary-search
  "Returns the index of value in the sorted array between lower-bound and
  upper-bound (inclusive), or -1 when value is absent."
  [array lower-bound upper-bound value]
  (let [r (quot (+ lower-bound upper-bound) 2)]
    (cond (= (nth array r) value) r
          (>= lower-bound upper-bound) -1
          (< (nth array r) value)
          (recur array (inc r) upper-bound value)
          :else
          (recur array lower-bound (dec r) value))))

(defn mat-bin-search
  "Searches each row of matrix for value with binary-search.
  Returns [row col], or [-1 -1] when value is absent."
  [value matrix]
  (if (= (get-in matrix [0 0]) value)
    [0 0]
    (loop [index 0]
      (if (and (< index (count matrix))
               (< (get-in matrix [index 0]) value))
        (let [row (nth matrix index)
              r (binary-search row 0 (dec (count row)) value)]
          (if (not= r -1)
            [index r]
            (recur (inc index))))
        [-1 -1]))))
