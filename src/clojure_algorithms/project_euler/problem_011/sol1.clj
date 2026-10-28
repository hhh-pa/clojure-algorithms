(ns clojure-algorithms.project-euler.problem-011.sol1
  "Project Euler Problem 11: largest product in a grid.

  What is the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the 20x20 array?

  This solution checks vertical, horizontal, and both diagonal products
  in a single pass (only works for square grids)."
  (:require [clojure.string :as str]))

(defn largest-product
  "Returns the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the square grid of numbers."
  [grid]
  (let [n-cols (count (first grid))
        n-rows (count grid)]
    (reduce max 0
            (for [i (range n-cols)
                  j (range (- n-rows 3))]
              (let [vert (* (get-in grid [j i])
                            (get-in grid [(+ j 1) i])
                            (get-in grid [(+ j 2) i])
                            (get-in grid [(+ j 3) i]))
                    horz (* (get-in grid [i j])
                            (get-in grid [i (+ j 1)])
                            (get-in grid [i (+ j 2)])
                            (get-in grid [i (+ j 3)]))
                    lr (if (< i (- n-cols 3))
                         (* (get-in grid [i j])
                            (get-in grid [(+ i 1) (+ j 1)])
                            (get-in grid [(+ i 2) (+ j 2)])
                            (get-in grid [(+ i 3) (+ j 3)]))
                         0)
                    rl (if (> i 2)
                         (* (get-in grid [i j])
                            (get-in grid [(- i 1) (+ j 1)])
                            (get-in grid [(- i 2) (+ j 2)])
                            (get-in grid [(- i 3) (+ j 3)]))
                         0)]
                (max vert horz lr rl))))))

(defn parse-grid
  "Parses grid text (rows of space-separated integers) into a vector of
  vectors of integers."
  [content]
  (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #"\s+")))
        (str/split-lines content)))

(defn solution
  "Returns the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the grid described by content, where
  content is the text of the grid file."
  [content]
  (largest-product (parse-grid content)))
