(ns clojure-algorithms.divide-and-conquer.strassen-matrix-multiplication
  "Matrix multiplication with Strassen's divide-and-conquer algorithm.

  Runs in O(n^log2(7)). Non power-of-two inputs are zero-padded, then
  the padding is trimmed. Note: the Python original returns its inputs
  unchanged for square inputs; this port multiplies them instead.
  Ported from TheAlgorithms/Python
  divide_and_conquer/strassen_matrix_multiplication.py.")

(defn matrix-dimensions
  "Return [rows cols] of matrix `m`."
  [m]
  [(count m) (count (first m))])

(defn- element-wise
  "Apply binary `op` element-wise to equally-sized matrices."
  [op a b]
  (mapv (fn [ra rb] (mapv op ra rb)) a b))

(defn matrix-addition
  "Element-wise sum of matrices `a` and `b`."
  [a b]
  (element-wise + a b))

(defn matrix-subtraction
  "Element-wise difference of matrices `a` and `b`."
  [a b]
  (element-wise - a b))

(defn default-matrix-multiplication
  "Naive multiplication, defined for 2x2 matrices."
  [a b]
  (when-not (and (= [2 2] (matrix-dimensions a))
                 (= [2 2] (matrix-dimensions b)))
    (throw (ex-info "Matrices are not 2x2" {})))
  [[(+ (* (get-in a [0 0]) (get-in b [0 0]))
       (* (get-in a [0 1]) (get-in b [1 0])))
    (+ (* (get-in a [0 0]) (get-in b [0 1]))
       (* (get-in a [0 1]) (get-in b [1 1])))]
   [(+ (* (get-in a [1 0]) (get-in b [0 0]))
       (* (get-in a [1 1]) (get-in b [1 0])))
    (+ (* (get-in a [1 0]) (get-in b [0 1]))
       (* (get-in a [1 1]) (get-in b [1 1])))]])

(defn split-matrix
  "Split an even-dimensioned matrix into [top-left top-right bot-left bot-right]."
  [a]
  (let [[rows cols] (matrix-dimensions a)]
    (when (or (odd? rows) (odd? cols))
      (throw (ex-info "Odd matrices are not supported!" {})))
    (let [mid (quot rows 2)
          top (subvec (vec a) 0 mid)
          bot (subvec (vec a) mid)
          left-of (fn [rows-v] (mapv #(subvec (vec %) 0 mid) rows-v))
          right-of (fn [rows-v] (mapv #(subvec (vec %) mid) rows-v))]
      [(left-of top) (right-of top) (left-of bot) (right-of bot)])))

(defn actual-strassen
  "Strassen product of square power-of-two matrices `a` and `b`."
  [a b]
  (if (= [2 2] (matrix-dimensions a))
    (default-matrix-multiplication a b)
    (let [[p q r s] (split-matrix a)
          [e f g h] (split-matrix b)
          t1 (actual-strassen p (matrix-subtraction f h))
          t2 (actual-strassen (matrix-addition p q) h)
          t3 (actual-strassen (matrix-addition r s) e)
          t4 (actual-strassen s (matrix-subtraction g e))
          t5 (actual-strassen (matrix-addition p s) (matrix-addition e h))
          t6 (actual-strassen (matrix-subtraction q s) (matrix-addition g h))
          t7 (actual-strassen (matrix-subtraction p r) (matrix-addition e f))
          top-left (matrix-addition
                    (matrix-subtraction (matrix-addition t5 t4) t2) t6)
          top-right (matrix-addition t1 t2)
          bot-left (matrix-addition t3 t4)
          bot-right (matrix-subtraction
                     (matrix-subtraction (matrix-addition t1 t5) t3) t7)]
      (vec (concat (mapv into top-left top-right)
                   (mapv into bot-left bot-right))))))

(defn- next-power-of-two
  "Smallest power of two >= `n`."
  [n]
  (int (Math/pow 2 (Math/ceil (/ (Math/log (max n 1)) (Math/log 2))))))

(defn- pad-to
  "Zero-pad matrix `m` to a `size` x `size` matrix."
  [m size]
  (let [[rows cols] (matrix-dimensions m)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (if (and (< i rows) (< j cols)) (get-in m [i j]) 0))
                  (range size)))
          (range size))))

(defn strassen
  "Multiply matrices `a` and `b` with Strassen's algorithm."
  [a b]
  (let [[arows acols] (matrix-dimensions a)
        [brows bcols] (matrix-dimensions b)]
    (when (not= acols brows)
      (throw (ex-info (str "Unable to multiply these matrices, please check "
                           "the dimensions.")
                      {:a (matrix-dimensions a) :b (matrix-dimensions b)})))
    (let [size (next-power-of-two (max arows acols brows bcols))
          product (actual-strassen (pad-to a size) (pad-to b size))]
      (mapv #(subvec % 0 bcols) (subvec product 0 arows)))))
