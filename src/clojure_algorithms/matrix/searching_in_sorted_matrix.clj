(ns clojure-algorithms.matrix.searching-in-sorted-matrix
  "Searches a key in a row- and column-wise sorted matrix.")

(defn search-in-a-sorted-matrix
  "Prints where key occurs in the m x n matrix, or that it is absent.
  Starts at the bottom-left corner and moves up or right."
  [mat m n key]
  (loop [i (dec m) j 0]
    (cond (or (neg? i) (>= j n))
          (println (str "Key " key " not found"))
          (= key (get-in mat [i j]))
          (println (str "Key " key " found at row- " (inc i)
                        " column- " (inc j)))
          (< key (get-in mat [i j]))
          (recur (dec i) j)
          :else
          (recur i (inc j)))))

(defn main
  "Reads a key from stdin and searches the sample matrix for it."
  []
  (let [mat [[2 5 7] [4 8 13] [9 11 15] [12 17 20]]]
    (print "Enter the element to be searched:")
    (flush)
    (let [x (Long/parseLong (read-line))]
      (println mat)
      (search-in-a-sorted-matrix mat (count mat) (count (first mat)) x))))
