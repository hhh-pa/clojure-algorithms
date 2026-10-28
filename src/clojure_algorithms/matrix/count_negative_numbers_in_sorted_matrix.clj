(ns clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix
  "Counts the negative numbers in a matrix whose rows and columns are
  sorted in decreasing order.
  Reference: https://leetcode.com/problems/count-negative-numbers-in-a-sorted-matrix")

(defn generate-large-matrix
  "Builds the 1000x2000 benchmark matrix."
  []
  (vec (for [i (range 1000)]
         (vec (range (- 1000 i) (- -1000 i) -1)))))

(def grid
  "Large benchmark matrix shared by the doctests."
  (generate-large-matrix))

(def test-grids
  "Grids used by the doctests, from small to large."
  [[[4 3 2 -1] [3 2 1 -1] [1 1 -1 -2] [-1 -1 -2 -3]]
   [[3 2] [1 0]]
   [[7 7 6]]
   [[7 7 6] [-1 -2 -3]]
   grid])

(defn validate-grid
  "Asserts that every row and column of grid is sorted in decreasing order."
  [grid]
  (assert (every? (fn [row] (= (vec row) (vec (reverse (sort row))))) grid))
  (assert (every? (fn [col] (= (vec col) (vec (reverse (sort col)))))
                  (apply map vector grid))))

(defn find-negative-index
  "Returns the index of the first negative number in the decreasing
  array, or the length of the array when there is none."
  [array]
  (let [v (vec array)]
    (if (or (empty? v) (neg? (nth v 0)))
      0
      (loop [left 0 right (dec (count v))]
        (if (> (inc right) left)
          (let [mid (quot (+ left right) 2)
                num (nth v mid)]
            (cond (and (neg? num) (>= (nth v (dec mid)) 0))
                  mid
                  (>= num 0)
                  (recur (inc mid) right)
                  :else
                  (recur left (dec mid))))
          (count v))))))

(defn count-negatives-binary-search
  "O(m log n) count using binary search on each row."
  [grid]
  (let [rows (count grid)
        cols (count (first grid))
        total (loop [i 0 bound cols total 0]
                (if (>= i rows)
                  total
                  (let [bound (find-negative-index
                               (subvec (vec (nth grid i)) 0 bound))]
                    (recur (inc i) bound (+ total bound)))))]
    (- (* rows cols) total)))

(defn count-negatives-brute-force
  "O(n^2) count iterating over every element."
  [grid]
  (count (for [row grid number row :when (neg? number)] number)))

(defn count-negatives-brute-force-with-break
  "Brute-force count stopping at the first negative of each row."
  [grid]
  (reduce +
          0
          (for [row grid]
            (let [v (vec row)
                  idx (first (keep-indexed (fn [i n] (when (neg? n) i)) v))]
              (if (nil? idx) 0 (- (count v) idx))))))

(defn benchmark
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))
