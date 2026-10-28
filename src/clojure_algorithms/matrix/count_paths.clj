(ns clojure-algorithms.matrix.count-paths
  "Counts distinct paths from the top-left to the bottom-right of a grid,
  where 1 marks a blocked cell and 0 a free one.")

(defn depth-first-search
  "Recursive backtracking depth-first search counting the paths from
  (row, col) to the bottom-right corner. visit holds the cells on the
  current path."
  [grid row col visit]
  (let [rows (count grid) cols (count (first grid))]
    (cond (or (neg? (min row col))
              (= row rows)
              (= col cols)
              (contains? visit [row col])
              (= 1 (get-in grid [row col])))
          0
          (and (= row (dec rows)) (= col (dec cols)))
          1
          :else
          (let [v (conj visit [row col])]
            (+ (depth-first-search grid (inc row) col v)
               (depth-first-search grid (dec row) col v)
               (depth-first-search grid row (inc col) v)
               (depth-first-search grid row (dec col) v))))))
