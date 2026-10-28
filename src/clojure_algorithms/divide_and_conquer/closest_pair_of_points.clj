(ns clojure-algorithms.divide-and-conquer.closest-pair-of-points
  "Closest pair of points in the plane.

  Divide-and-conquer algorithm running in O(n log n). Points are [x y]
  pairs. Distances are computed squared internally; `closest-pair-of-points`
  returns the Euclidean distance.
  Ported from TheAlgorithms/Python divide_and_conquer/closest_pair_of_points.py.")

(defn euclidean-distance-sqr
  "Squared Euclidean distance between points `p1` and `p2`."
  [[x1 y1] [x2 y2]]
  (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

(defn column-based-sort
  "Sort `points` by coordinate `column` (0 for x, 1 for y)."
  [points column]
  (sort-by #(nth % column) points))

(defn dis-between-closest-pair
  "Brute-force squared distance of the closest pair in `points`."
  [points]
  (let [pts (vec points)
        n (count pts)]
    (reduce (fn [best i]
              (reduce (fn [b j]
                        (min b (euclidean-distance-sqr (pts i) (pts j))))
                      best
                      (range (inc i) n)))
            Double/POSITIVE_INFINITY
            (range (max 0 (dec n))))))

(defn dis-between-closest-in-strip
  "Closest squared distance inside a y-sorted `strip`, seeded with `min-dis`.
  Each point needs comparing against at most the 6 preceding points."
  [strip min-dis]
  (let [pts (vec strip)
        n (count pts)]
    (reduce (fn [best i]
              (reduce (fn [b j]
                        (min b (euclidean-distance-sqr (pts i) (pts j))))
                      best
                      (range (max 0 (- i 6)) i)))
            min-dis
            (range (min 6 (max 0 (dec n))) n))))

(declare closest-pair-of-points-sqr)

(defn closest-pair-of-points-sqr
  "Squared closest-pair distance via divide and conquer.
  `xs`/`ys` are the points sorted by x and by y respectively."
  [xs ys]
  (let [xs (vec xs)
        n (count xs)]
    (if (<= n 3)
      (dis-between-closest-pair xs)
      (let [mid (quot n 2)
            left-xs (subvec xs 0 mid)
            right-xs (subvec xs mid)
            in-left? (set left-xs)
            left-ys (filterv in-left? ys)
            right-ys (filterv (complement in-left?) ys)
            best (min (closest-pair-of-points-sqr left-xs left-ys)
                      (closest-pair-of-points-sqr right-xs right-ys))
            mid-x (first (xs mid))
            strip (filterv #(<= (Math/abs (double (- (first %) mid-x)))
                                (Math/sqrt best))
                           ys)]
        (dis-between-closest-in-strip strip best)))))

(defn closest-pair-of-points
  "Euclidean distance between the closest pair of `points`."
  [points]
  (let [pts (vec points)]
    (Math/sqrt
     (closest-pair-of-points-sqr (column-based-sort pts 0)
                                 (column-based-sort pts 1)))))
