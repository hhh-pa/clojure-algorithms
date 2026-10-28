(ns clojure-algorithms.data-structures.arrays.sparse-table
  "Port of data_structures/arrays/sparse_table.py.
  Sparse table for range-minimum queries.")

(defn- log2-floor [n]
  (int (/ (Math/log n) (Math/log 2))))

(defn build-sparse-table
  "Precompute the range-minimum sparse table for NUMBER-LIST."
  [number-list]
  (when (empty? number-list)
    (throw (ex-info "empty number list not allowed" {:type :value-error})))
  (let [v (vec number-list)
        length (count v)
        nrows (inc (log2-floor length))
        table (vec (mapv (fn [j]
                           (if (zero? j)
                             v
                             (vec (repeat length 0))))
                         (range nrows)))]
    (loop [table table j 1]
      (if (> (bit-shift-left 1 j) length)
        table
        (recur (loop [table table i 0]
                 (if (>= (+ i (bit-shift-left 1 j) -1) length)
                   table
                   (recur (assoc-in table [j i]
                                    (min (nth (nth table (dec j))
                                              (+ i (bit-shift-left 1 (dec j))))
                                         (nth (nth table (dec j)) i)))
                          (inc i))))
               (inc j))))))

(defn sparse-query
  "Minimum of NUMBER-LIST between LEFT-BOUND and RIGHT-BOUND inclusive,
  using a table from build-sparse-table."
  [sparse-table left-bound right-bound]
  (when (>= left-bound (count (first sparse-table)))
    (throw (ex-info "list index out of range" {:type :index-error})))
  (when (or (neg? left-bound) (>= right-bound (count (first sparse-table))))
    (throw (ex-info "list index out of range" {:type :index-error})))
  (let [j (log2-floor (inc (- right-bound left-bound)))]
    (min (nth (nth sparse-table j) (inc (- right-bound (bit-shift-left 1 j))))
         (nth (nth sparse-table j) left-bound))))
