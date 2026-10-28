(ns clojure-algorithms.matrix.pascal-triangle
  "Generates Pascal's triangle, a triangular array of binomial coefficients.
  https://en.wikipedia.org/wiki/Pascal%27s_triangle"
  (:require [clojure.string :as str]))

(defn- check-num-rows
  "Throws unless num-rows is a non-negative integer."
  [num-rows]
  (when-not (integer? num-rows)
    (throw (ex-info "The input value of 'num_rows' should be 'int'"
                    {:type :TypeError})))
  (when (neg? num-rows)
    (throw (ex-info (str "The input value of 'num_rows' should be greater "
                         "than or equal to 0")
                    {:type :ValueError}))))

(defn calculate-current-element
  "Fills position current-col-idx of current-row from the previous triangle
  row. Returns the updated row (upstream mutates it in place)."
  [triangle current-row current-row-idx current-col-idx]
  (assoc (vec current-row)
         current-col-idx
         (+ (get-in triangle [(dec current-row-idx) (dec current-col-idx)])
            (get-in triangle [(dec current-row-idx) current-col-idx]))))

(defn populate-current-row
  "Builds row current-row-idx of the triangle."
  [triangle current-row-idx]
  (let [init (assoc (vec (repeat (inc current-row-idx) -1))
                    0 1
                    current-row-idx 1)]
    (reduce (fn [row c]
              (calculate-current-element triangle row current-row-idx c))
            init
            (range 1 current-row-idx))))

(defn generate-pascal-triangle
  "Returns the first num-rows rows of Pascal's triangle."
  [num-rows]
  (check-num-rows num-rows)
  (if (zero? num-rows)
    []
    (reduce (fn [triangle idx] (conj triangle (populate-current-row triangle idx)))
            []
            (range num-rows))))

(defn generate-pascal-triangle-optimized
  "Returns the first num-rows rows of Pascal's triangle, computing only
  half of each row and mirroring it."
  [num-rows]
  (check-num-rows num-rows)
  (if (zero? num-rows)
    []
    (loop [result [[1]] row-index 1]
      (if (>= row-index num-rows)
        result
        (let [temp-row (vec (concat [0] (peek result) [0]))
              row-length (inc row-index)
              distinct (+ (quot row-length 2) (mod row-length 2))
              first-half (mapv (fn [i]
                                 (+ (nth temp-row (dec i)) (nth temp-row i)))
                               (range 1 (inc distinct)))
              second-half (vec (reverse (subvec first-half 0 (quot (inc row-index) 2))))]
          (recur (conj result (vec (concat first-half second-half)))
                 (inc row-index)))))))

(defn print-pascal-triangle
  "Prints the first num-rows rows of Pascal's triangle."
  [num-rows]
  (let [triangle (generate-pascal-triangle num-rows)]
    (doseq [r (range num-rows)]
      (print (apply str (repeat (- num-rows r 1) " ")))
      (println (str/join " " (nth triangle r))))))

(defn benchmark
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))
