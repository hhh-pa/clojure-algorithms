(ns clojure-algorithms.geometry.rotating-calipers
  "Polygon diameter (max point distance) via rotating calipers.")

(defrecord Point [x y])

(defn cross-product
  "2D cross product of (origin->a) and (origin->b)."
  [origin a b]
  (- (* (- (:x a) (:x origin)) (- (:y b) (:y origin)))
     (* (- (:y a) (:y origin)) (- (:x b) (:x origin)))))

(defn distance-squared
  "Squared Euclidean distance between points a and b."
  [a b]
  (+ (* (- (:x a) (:x b)) (- (:x a) (:x b)))
     (* (- (:y a) (:y b)) (- (:y a) (:y b)))))

(defn- half-hull [pts]
  (reduce (fn [hull pt]
            (loop [h (conj hull pt)]
              (if (and (>= (count h) 3)
                       (<= (cross-product (nth h (- (count h) 3))
                                          (nth h (- (count h) 2))
                                          (nth h (- (count h) 1))) 0.0))
                (recur (into (subvec h 0 (- (count h) 2))
                             [(nth h (dec (count h)))]))
                h)))
          [] pts))

(defn convex-hull
  "Convex hull vertices in counter-clockwise order (monotone chain)."
  [points]
  (let [pts (vec (sort-by (juxt :x :y) (distinct points)))]
    (if (<= (count pts) 1)
      pts
      (vec (concat (pop (half-hull pts)) (pop (half-hull (rseq pts))))))))

(defn rotating-calipers
  "Returns [max-distance [point-a point-b]] for the diameter of points.
   Throws when fewer than 2 points are given."
  [points]
  (when (< (count points) 2)
    (throw (IllegalArgumentException.
            "At least 2 points are required to compute polygon diameter.")))
  (let [hull (convex-hull points)
        n (count hull)]
    (cond
      (= n 1) [0.0 [(nth hull 0) (nth hull 0)]]
      (= n 2) [(Math/hypot (- (:x (nth hull 0)) (:x (nth hull 1)))
                           (- (:y (nth hull 0)) (:y (nth hull 1))))
               [(nth hull 0) (nth hull 1)]]
      :else
      (let [advance (fn [i j k]
                      (loop [k k]
                        (if (> (cross-product (nth hull i) (nth hull j)
                                              (nth hull (mod (inc k) n)))
                               (cross-product (nth hull i) (nth hull j)
                                              (nth hull k)))
                          (recur (mod (inc k) n)) k)))
            k0 (advance 0 1 1)]
        (loop [i 0 k k0 best-sq 0.0 best [(nth hull 0) (nth hull 1)]]
          (if (>= i n)
            [(Math/sqrt best-sq) best]
            (let [j (mod (inc i) n)
                  k2 (advance i j k)
                  [sq pair]
                  (reduce (fn [[bs bp] [p q]]
                            (let [d (distance-squared p q)]
                              (if (> d bs) [d [p q]] [bs bp])))
                          [best-sq best]
                          (for [p [(nth hull i) (nth hull j)]
                                q [(nth hull k2) (nth hull (mod (inc k2) n))]]
                            [p q]))]
              (recur (inc i) k2 sq pair))))))))
