(ns clojure-algorithms.project-euler.problem-011.sol2
  "Project Euler Problem 11: largest product in a grid.

  What is the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the 20x20 array?

  This solution scans each of the four directions (right, down, and both
  diagonals) in its own pass."
  (:require [clojure.string :as str]))

(defn parse-grid
  "Parses grid text (rows of space-separated integers) into a vector of
  vectors of integers."
  [content]
  (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #"\s+")))
        (str/split-lines content)))

(defn largest-product
  "Returns the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the grid of numbers."
  [grid]
  (let [rows (count grid)
        cols (count (first grid))
        products
        (concat
         ;; right
         (for [i (range rows) j (range (- cols 3))]
           (* (get-in grid [i j]) (get-in grid [i (+ j 1)])
              (get-in grid [i (+ j 2)]) (get-in grid [i (+ j 3)])))
         ;; down
         (for [i (range (- rows 3)) j (range cols)]
           (* (get-in grid [i j]) (get-in grid [(+ i 1) j])
              (get-in grid [(+ i 2) j]) (get-in grid [(+ i 3) j])))
         ;; diagonal 1
         (for [i (range (- rows 3)) j (range (- cols 3))]
           (* (get-in grid [i j]) (get-in grid [(+ i 1) (+ j 1)])
              (get-in grid [(+ i 2) (+ j 2)]) (get-in grid [(+ i 3) (+ j 3)])))
         ;; diagonal 2
         (for [i (range (- rows 3)) j (range 3 cols)]
           (* (get-in grid [i j]) (get-in grid [(+ i 1) (- j 1)])
              (get-in grid [(+ i 2) (- j 2)]) (get-in grid [(+ i 3) (- j 3)]))))]
    (reduce max 0 products)))

(defn solution
  "Returns the greatest product of four adjacent numbers (horizontally,
  vertically, or diagonally) in the grid described by content, where
  content is the text of the grid file."
  [content]
  (largest-product (parse-grid content)))
