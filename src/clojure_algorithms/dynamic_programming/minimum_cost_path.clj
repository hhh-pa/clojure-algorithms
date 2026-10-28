(ns clojure-algorithms.dynamic-programming.minimum-cost-path
  "Minimum cost path from top-left to bottom-right moving only
  down or right.
  Ported from TheAlgorithms/Python dynamic_programming/minimum_cost_path.py.")

(defn minimum-cost-path
  "Minimum path cost through `matrix` (not mutated)."
  [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))
        dp (atom (mapv vec matrix))]
    (doseq [j (range 1 cols)]
      (swap! dp update-in [0 j] + (get-in @dp [0 (dec j)])))
    (doseq [i (range 1 rows)]
      (swap! dp update-in [i 0] + (get-in @dp [(dec i) 0])))
    (doseq [i (range 1 rows)
            j (range 1 cols)]
      (swap! dp update-in [i j] + (min (get-in @dp [(dec i) j])
                                       (get-in @dp [i (dec j)]))))
    (get-in @dp [(dec rows) (dec cols)])))
