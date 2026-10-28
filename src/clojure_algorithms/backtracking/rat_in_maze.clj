(ns clojure-algorithms.backtracking.rat-in-maze
  "Rat in a Maze: navigate an n x n grid of walls (1) and open paths (0)
  from a source cell to a destination cell. The solution matrix marks the
  found path with 0s and everything else with 1s.")

(defn run-maze
  "Recursive search from (i, j), trying down, right, up, then left.
  Returns [solved? solutions]."
  [maze i j destination-row destination-column solutions]
  (let [size (count maze)]
    (cond
      (and (= i destination-row) (= j destination-column)
           (zero? (get-in maze [i j] 1)))
      [true (assoc-in solutions [i j] 0)]

      (not (and (<= 0 i (dec size)) (<= 0 j (dec size))))
      [false solutions]

      (not (and (= 1 (get-in solutions [i j]))
                (zero? (get-in maze [i j]))))
      [false solutions]

      :else
      (let [marked (assoc-in solutions [i j] 0)]
        (or (some (fn [[ni nj]]
                    (let [[ok sols] (run-maze maze ni nj destination-row
                                              destination-column marked)]
                      (when ok [true sols])))
                  [[(inc i) j] [i (inc j)] [(dec i) j] [i (dec j)]])
            [false solutions])))))

(defn solve-maze
  "Returns the solution matrix for a path from (`source-row`,
  `source-column`) to (`destination-row`, `destination-column`).
  Throws when coordinates are invalid or no path exists."
  [maze source-row source-column destination-row destination-column]
  (let [size (count maze)
        valid? (fn [r c] (and (<= 0 r (dec size)) (<= 0 c (dec size))))]
    (when (not (and (valid? source-row source-column)
                    (valid? destination-row destination-column)))
      (throw (ex-info "Invalid source or destination coordinates" {})))
    (let [[solved solutions] (run-maze maze source-row source-column
                                       destination-row destination-column
                                       (vec (repeat size (vec (repeat size 1)))))]
      (if solved
        solutions
        (throw (ex-info "No solution exists!" {}))))))
