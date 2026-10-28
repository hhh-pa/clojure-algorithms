(ns clojure-algorithms.matrix.matrix-multiplication-recursion
  "Matrix multiplication, including a recursive algorithm.
  https://en.wikipedia.org/wiki/Matrix_multiplication")

(def Matrix
  "Placeholder for the Python `Matrix = list[list[int]]` type alias."
  nil)

(def matrix-1-to-4 [[1 2] [3 4]])

(def matrix-5-to-8 [[5 6] [7 8]])

(def matrix-5-to-9-high [[5 6] [7 8] [9]])

(def matrix-5-to-9-wide [[5 6] [7 8 9]])

(def matrix-count-up
  [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(def matrix-unordered
  [[5 8 1 2] [6 7 3 0] [4 5 9 1] [2 6 10 14]])

(def matrices
  [matrix-1-to-4 matrix-5-to-8 matrix-5-to-9-high
   matrix-5-to-9-wide matrix-count-up matrix-unordered])

(defn is-square
  "True when every row of matrix has as many elements as there are rows."
  [matrix]
  (let [n (count matrix)]
    (every? #(= n (count %)) matrix)))

(defn matrix-multiply
  "Multiplies matrix-a by matrix-b with a comprehension."
  [matrix-a matrix-b]
  (let [cols (apply map vector matrix-b)]
    (mapv (fn [row]
            (mapv (fn [col] (reduce +' 0 (map *' row col))) cols))
          matrix-a)))

(defn matrix-multiply-recursive
  "Multiplies two square matrices of equal size by recursing over
  the (i, j, k) index triple."
  [matrix-a matrix-b]
  (if (or (empty? matrix-a) (empty? matrix-b))
    []
    (do
      (when-not (and (= (count matrix-a) (count matrix-b))
                     (is-square matrix-a)
                     (is-square matrix-b))
        (throw (ex-info "Invalid matrix dimensions" {:type :ValueError})))
      (let [result (vec (repeat (count matrix-a)
                                (vec (repeat (count (first matrix-b)) 0))))]
        (letfn [(multiply [i j k result]
                  (cond (>= i (count matrix-a)) result
                        (>= j (count (first matrix-b)))
                        (recur (inc i) 0 0 result)
                        (>= k (count matrix-b))
                        (recur i (inc j) 0 result)
                        :else
                        (recur i j (inc k)
                               (update-in result [i j] +'
                                          (* (get-in matrix-a [i k])
                                             (get-in matrix-b [k j]))))))]
          (multiply 0 0 0 result))))))
