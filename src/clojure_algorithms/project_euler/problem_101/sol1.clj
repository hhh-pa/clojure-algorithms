(ns clojure-algorithms.project-euler.problem-101.sol1
  "Project Euler 101: optimum polynomial.

  We define OP(k, n) to be the nth term of the optimum polynomial
  generating function for the first k terms of a sequence. Consider the
  tenth degree polynomial generating function 1 - n + n^2 - ... + n^10
  and find the sum of FITs for the BOPs.")

(defn- round-to
  "Rounds x to decimals decimal places."
  [x decimals]
  (let [scale (Math/pow 10 decimals)]
    (/ (Math/round (* x scale)) scale)))

(defn solve
  "Solves the linear system of equations Ax = b (A = matrix, b = vector)
  for x using Gaussian elimination and back substitution. Assumes A is
  an invertible square matrix and b is a column vector of the same
  height. Returns a column vector."
  [matrix vector]
  (let [size (count matrix)
        aug (make-array Double/TYPE size (inc size))]
    (doseq [row (range size) col (range size)]
      (aset aug row col (double (nth (nth matrix row) col))))
    (doseq [row (range size)]
      (aset aug row size (double (nth (nth vector row) 0))))
    (let [get-cell (fn [r c] (aget ^doubles (aget aug r) c))
          set-cell! (fn [r c v] (aset aug r c (double v)))
          swap-rows! (fn [r1 r2]
                       (doseq [c (range (inc size))]
                         (let [tmp (get-cell r1 c)]
                           (set-cell! r1 c (get-cell r2 c))
                           (set-cell! r2 c tmp))))]
      ;; Forward elimination with partial pivoting.
      (loop [row 0 col 0]
        (when (and (< row size) (< col size))
          (let [pivot-row (last (sort-by
                                 (fn [r] [(Math/abs (get-cell r col)) r])
                                 (range col size)))]
            (if (zero? (get-cell pivot-row col))
              (recur row (inc col))
              (do
                (swap-rows! row pivot-row)
                (doseq [row2 (range (inc row) size)]
                  (let [ratio (/ (get-cell row2 col) (get-cell row col))]
                    (set-cell! row2 col 0)
                    (doseq [col2 (range (inc col) (inc size))]
                      (set-cell! row2 col2
                                 (- (get-cell row2 col2)
                                    (* (get-cell row col2) ratio))))))
                (recur (inc row) (inc col)))))))
      ;; Back substitution.
      (doseq [col (range 1 size) row (range col)]
        (let [ratio (/ (get-cell row col) (get-cell col col))]
          (doseq [col2 (range col (inc size))]
            (set-cell! row col2
                       (- (get-cell row col2)
                          (* (get-cell col col2) ratio))))))
      ;; Round to get rid of numbers like 2.000000000000004.
      (vec (for [row (range size)]
             [(round-to (/ (get-cell row size) (get-cell row row)) 10)])))))

(defn- int-pow
  "Returns base raised to exp as an exact integer."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn interpolate
  "Given a list of data points (1,y0),(2,y1), ..., returns a function
  that interpolates them. The coefficients of the interpolating
  polynomial are found by solving the linear system for x = 1, 2, 3..."
  [y-list]
  (let [size (count y-list)
        matrix (vec (for [x-val (range size)]
                      (vec (for [col (range size)]
                             (int-pow (inc x-val) (- size col 1))))))
        vector (vec (for [y-val y-list] [y-val]))
        coeffs (solve matrix vector)]
    (fn [var]
      (reduce + (for [x-val (range size)]
                  (* (Math/round ^double (nth (nth coeffs x-val) 0))
                     (int-pow var (- size x-val 1))))))))

(defn question-function
  "The generating function u(n) = 1 - n + n^2 - ... + n^10 specified in
  the question."
  [variable]
  (reduce + (for [k (range 11)]
              (* (if (even? k) 1 -1) (int-pow variable k)))))

(defn solution
  "Finds the sum of the FITs of the BOPs: for each interpolating
  polynomial of order 1, 2, ..., order, finds the first x such that the
  value of the polynomial at x does not equal (func x)."
  ([] (solution question-function 10))
  ([func] (solution func 10))
  ([func order]
   (let [data-points (vec (for [x-val (range 1 (inc order))]
                            (func x-val)))
         polynomials (vec (for [max-coeff (range 1 (inc order))]
                            (interpolate (subvec data-points 0 max-coeff))))]
     (reduce + (for [poly polynomials]
                 (loop [x-val 1]
                   (if (= (func x-val) (poly x-val))
                     (recur (inc x-val))
                     (poly x-val))))))))
