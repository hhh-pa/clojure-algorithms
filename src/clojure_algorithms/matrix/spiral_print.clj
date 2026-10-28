(ns clojure-algorithms.matrix.spiral-print
  "Prints and traverses a matrix in clockwise spiral order.")

(defn- row-like?
  [row]
  (or (sequential? row) (string? row)))

(defn check-matrix
  "True when matrix is a non-empty 2D grid whose rows all have equal
  length. Throws a TypeError for non-matrix inputs, mirroring upstream."
  [matrix]
  (when-not (sequential? matrix)
    (throw (ex-info "Not a valid matrix" {:type :TypeError})))
  (if (empty? matrix)
    false
    (do
      (doseq [row matrix]
        (when-not (row-like? row)
          (throw (ex-info "Not a valid matrix" {:type :TypeError}))))
      (apply = (map count matrix)))))

(defn- slice-clamp
  "Slice of v from from (inclusive) to to (exclusive), clamped like
  Python slicing."
  [v from to]
  (let [n (count v)
        from (min (max from 0) n)
        to (min (max to 0) n)]
    (if (<= to from)
      []
      (subvec (vec v) from to))))

(defn spiral-print-clockwise
  "Prints the elements of the matrix in clockwise spiral order,
  peeling one layer per recursive call."
  [a]
  (if (and (check-matrix a) (pos? (count a)))
    (let [m (mapv vec a)
          rows (count m)]
      (if (sequential? (first m))
        (let [cols (count (first m))]
          (doseq [i (range cols)]
            (println (get-in m [0 i])))
          (doseq [i (range 1 rows)]
            (println (get-in m [i (dec cols)])))
          (when (> rows 1)
            (doseq [i (range (- cols 2) -1 -1)]
              (println (get-in m [(dec rows) i]))))
          (doseq [i (range (- rows 2) 0 -1)]
            (println (get-in m [i 0])))
          (let [remain (mapv #(slice-clamp % 1 (dec cols))
                             (slice-clamp m 1 (dec rows)))]
            (when (pos? (count remain))
              (spiral-print-clockwise remain))))
        (doseq [datum m]
          (println datum))))
    (println "Not a valid matrix")))

(defn spiral-traversal
  "Returns the elements of the matrix in clockwise spiral order by
  taking the first row, then recursing on the reversed transpose."
  [matrix]
  (if (seq matrix)
    (let [tail (rest matrix)]
      (vec (concat (first matrix)
                   (spiral-traversal (if (seq tail)
                                       (vec (reverse (apply mapv vector tail)))
                                       [])))))
    []))
