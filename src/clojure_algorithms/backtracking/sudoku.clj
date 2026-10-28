(ns clojure-algorithms.backtracking.sudoku
  "9x9 Sudoku solver via backtracking: try digits 1-9 in the first empty
  cell and recurse, undoing placements that lead nowhere.")

(def initial-grid
  "A solvable example puzzle (0 marks an empty cell)."
  [[3 0 6 5 0 8 4 0 0]
   [5 2 0 0 0 0 0 0 0]
   [0 8 7 0 0 0 0 3 1]
   [0 0 3 0 1 0 0 8 0]
   [9 0 0 8 6 3 0 0 5]
   [0 5 0 0 9 0 6 0 0]
   [1 3 0 0 0 0 2 5 0]
   [0 0 0 0 0 0 0 7 4]
   [0 0 5 2 0 6 3 0 0]])

(def no-solution
  "An example grid admitting no solution."
  [[5 0 6 5 0 8 4 0 3]
   [5 2 0 0 0 0 0 0 2]
   [1 8 7 0 0 0 0 3 1]
   [0 0 3 0 1 0 0 8 0]
   [9 0 0 8 6 3 0 0 5]
   [0 5 0 0 9 0 6 0 0]
   [1 3 0 0 0 0 2 5 0]
   [0 0 0 0 0 0 0 7 4]
   [0 0 5 2 0 6 3 0 0]])

(defn safe?
  "Returns true when digit `n` duplicates nothing in `row`, `column` or
  their 3x3 box."
  [grid row column n]
  (and (not-any? #(= n %) (nth grid row))
       (not-any? #(= n (nth % column)) grid)
       (not-any? #(= n %)
                 (for [i (range 3) j (range 3)]
                   (get-in grid [(+ (- row (mod row 3)) i)
                                 (+ (- column (mod column 3)) j)])))))

(defn find-empty-location
  "Returns [row column] of the first empty (0) cell, or nil when full."
  [grid]
  (some (fn [[i row]]
          (some (fn [[j cell]]
                  (when (zero? cell) [i j]))
                (map-indexed vector row)))
        (map-indexed vector grid)))

(defn sudoku
  "Returns the solved grid, or nil when no solution exists."
  [grid]
  (if-let [[row column] (find-empty-location grid)]
    (some (fn [digit]
            (when (safe? grid row column digit)
              (sudoku (assoc-in grid [row column] digit))))
          (range 1 10))
    grid))

(defn print-solution
  "Prints `grid` as a 9x9 block of digits."
  [grid]
  (doseq [row grid]
    (println (apply str (map #(str % " ") row)))))
