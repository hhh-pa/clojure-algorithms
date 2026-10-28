(ns clojure-algorithms.project-euler.problem-082.sol1
  "Project Euler 82: path sum, three ways.

  Find the minimal path sum from the left column to the right column by
  starting in any cell in the left column and finishing in any cell in
  the right column, moving only up, down, and right.

  This port takes the matrix as a string (comma-separated rows) instead
  of reading a file, so the pure logic can be tested with tiny data."
  (:require [clojure.string :as str]))

(defn solution
  "Returns the minimal path sum from the left column to the right column
  of the matrix described by content."
  [content]
  (let [matrix (mapv #(mapv parse-long (str/split % #","))
                     (str/split-lines (str/trim content)))
        rows (count matrix)
        cols (count (first matrix))]
    (loop [j 1 prev (mapv #(nth % 0) matrix)]
      (if (= j cols)
        (apply min prev)
        (let [col (mapv #(nth % j) matrix)
              right (mapv + prev col)
              down (reduce (fn [acc i]
                             (assoc acc i (min (acc i)
                                               (+ (acc (dec i)) (col i)))))
                           right (range 1 rows))
              both (reduce (fn [acc i]
                             (assoc acc i (min (acc i)
                                               (+ (acc (inc i)) (col i)))))
                           down (range (- rows 2) -1 -1))]
          (recur (inc j) both))))))
