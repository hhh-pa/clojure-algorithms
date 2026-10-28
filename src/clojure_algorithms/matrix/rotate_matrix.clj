(ns clojure-algorithms.matrix.rotate-matrix
  "Rotates a square matrix by 90, 180 and 270 degrees counterclockwise."
  (:require [clojure.string :as str]))

(defn make-matrix
  "Builds the n x n sample matrix filled with 1..n*n.
  Defaults to 4x4; 0 also yields the default."
  ([] (make-matrix 4))
  ([row-size]
   (let [n (let [a (abs row-size)] (if (zero? a) 4 a))]
     (mapv (fn [y] (mapv (fn [x] (inc (+ x (* y n)))) (range n)))
           (range n)))))

(defn transpose
  "Mirrors the matrix along its main diagonal."
  [matrix]
  (apply mapv vector matrix))

(defn reverse-row
  "Reverses the order of the rows."
  [matrix]
  (vec (reverse matrix)))

(defn reverse-column
  "Reverses the order of the elements within each row."
  [matrix]
  (mapv (comp vec reverse) matrix))

(defn rotate-90
  "Rotates the matrix 90 degrees counterclockwise."
  [matrix]
  (reverse-row (transpose matrix)))

(defn rotate-180
  "Rotates the matrix 180 degrees."
  [matrix]
  (reverse-row (reverse-column matrix)))

(defn rotate-270
  "Rotates the matrix 270 degrees counterclockwise."
  [matrix]
  (reverse-column (transpose matrix)))

(defn print-matrix
  "Prints the matrix one row per line."
  [matrix]
  (doseq [row matrix]
    (println (str/join " " row))))
