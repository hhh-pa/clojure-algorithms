(ns clojure-algorithms.matrix.median-matrix
  "Median of a matrix. https://en.wikipedia.org/wiki/Median")

(defn median
  "Flattens matrix, sorts it and returns the middle element
  (the lower middle for an even number of elements)."
  [matrix]
  (let [linear (sort (mapcat seq matrix))
        mid (quot (dec (count linear)) 2)]
    (nth linear mid)))
