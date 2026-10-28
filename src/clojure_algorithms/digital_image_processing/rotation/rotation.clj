(ns clojure-algorithms.digital-image-processing.rotation.rotation
  "Pure port of digital_image_processing/rotation/rotation.py.
  The Python version only wraps cv2.getAffineTransform/warpAffine;
  this port implements that math directly: solve for the 2x3 affine
  matrix mapping three source points to three destination points,
  then nearest-sample warp the image (out-of-bounds reads yield 0).")

(defn- solve-linear
  "Solve the square linear system A x = b via Gaussian elimination."
  [a b]
  (let [n (count a)
        aug (mapv (fn [row bi] (conj (vec (map double row)) (double bi)))
                  a b)]
    (loop [m aug col 0]
      (if (>= col n)
        (loop [x (vec (repeat n 0.0)) i (dec n)]
          (if (< i 0)
            x
            (let [row (nth m i)
                  known (reduce + 0 (map * (subvec row (inc i) n)
                                         (subvec x (inc i))))]
              (recur (assoc x i (/ (- (nth row n) known) (nth row i)))
                     (dec i)))))
        (let [pivot (apply max-key #(Math/abs (double (nth (nth m %) col)))
                           (range col n))
              m (assoc m col (nth m pivot) pivot (nth m col))
              piv (nth (nth m col) col)
              m (reduce (fn [m r]
                          (let [factor (/ (nth (nth m r) col) piv)]
                            (assoc m r (mapv - (nth m r)
                                             (mapv #(* factor %) (nth m col))))))
                        m (range (inc col) n))]
          (recur m (inc col)))))))

(defn get-affine-transform
  "2x3 affine matrix mapping the 3 source points PT1 to PT2."
  [pt1 pt2]
  (let [rows (mapcat (fn [[sx sy] [dx dy]]
                       [[[sx sy 1 0 0 0] dx]
                        [[0 0 0 sx sy 1] dy]])
                     pt1 pt2)
        a (mapv first rows)
        b (mapv second rows)
        [a11 a12 b1 a21 a22 b2] (solve-linear a b)]
    [[a11 a12 b1] [a21 a22 b2]]))

(defn- invert-affine
  "Inverse of a 2x3 affine matrix."
  [[[a b c] [d e f]]]
  (let [det (- (* a e) (* b d))]
    [[(/ e det) (/ (- b) det) (/ (- (* b f) (* e c)) det)]
     [(/ (- d) det) (/ a det) (/ (- (* d c) (* a f)) det)]]))

(defn warp-affine
  "Warp IMG by affine MATRIX into a ROWS x COLS image (nearest)."
  [img matrix rows cols]
  (let [h (count img)
        w (count (first img))
        [[a b c] [d e f]] (invert-affine matrix)
        sample (fn [i j]
                 (let [sx (int (Math/round (double (+ (* a j) (* b i) c))))
                       sy (int (Math/round (double (+ (* d j) (* e i) f))))]
                   (if (and (<= 0 sx (dec w)) (<= 0 sy (dec h)))
                     (nth (nth img sy) sx)
                     0)))]
    (mapv (fn [i] (mapv #(sample i %) (range cols))) (range rows))))

(defn get-rotation
  "IMG warped by the affine transform mapping PT1 to PT2."
  [img pt1 pt2 rows cols]
  (warp-affine img (get-affine-transform pt1 pt2) rows cols))
