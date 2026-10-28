(ns clojure-algorithms.linear-algebra.lu-decomposition
  "LU decomposition of a square matrix into lower- and upper-triangular
  factors. See https://en.wikipedia.org/wiki/LU_decomposition")

(defn lower-upper-decomposition
  "Returns [lower upper] such that table = lower*upper. Throws when
  table is not square or no LU decomposition exists."
  [table]
  (let [rows (count table)
        cols (count (first table))]
    (when (not= rows cols)
      (throw (ex-info (format "'table' has to be of square shaped array but got a %dx%d array"
                              rows cols) {})))
    (loop [i 0
           lower (mapv #(mapv (fn [_] 0.0) %) table)
           upper (mapv #(mapv (fn [_] 0.0) %) table)]
      (if (= i cols)
        [lower upper]
        (let [lower (reduce (fn [lower j]
                              (let [total (reduce + 0.0 (map * (subvec (nth lower i) 0 i)
                                                             (mapv #(nth % j) (subvec upper 0 i))))]
                                (when (zero? (get-in upper [j j]))
                                  (throw (ex-info "No LU decomposition exists" {:type :arithmetic})))
                                (assoc-in lower [i j]
                                          (/ (- (get-in table [i j]) total)
                                             (get-in upper [j j])))))
                            lower (range i))
              lower (assoc-in lower [i i] 1.0)
              upper (reduce (fn [upper j]
                              (let [total (reduce + 0.0 (map * (subvec (nth lower i) 0 i)
                                                             (mapv #(nth % j) (subvec upper 0 i))))]
                                (assoc-in upper [i j]
                                          (- (get-in table [i j]) total))))
                            upper (range i cols))]
          (recur (inc i) lower upper))))))
