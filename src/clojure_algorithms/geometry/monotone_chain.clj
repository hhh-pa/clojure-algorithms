(ns clojure-algorithms.geometry.monotone-chain
  "Convex hull via Andrew's monotone chain algorithm.")

(defrecord Point [x y])

(defn cross-product-direction
  "2D cross product of (origin->a) and (origin->b). Positive means
   counter-clockwise, negative clockwise, zero collinear."
  [origin a b]
  (- (* (- (:x a) (:x origin)) (- (:y b) (:y origin)))
     (* (- (:y a) (:y origin)) (- (:x b) (:x origin)))))

(defn- half-hull [pts]
  (reduce (fn [hull pt]
            (loop [h (conj hull pt)]
              (if (and (>= (count h) 3)
                       (<= (cross-product-direction (nth h (- (count h) 3))
                                                    (nth h (- (count h) 2))
                                                    (nth h (- (count h) 1)))
                           0))
                (recur (into (subvec h 0 (- (count h) 2))
                             [(nth h (dec (count h)))]))
                h)))
          [] pts))

(defn monotone-chain
  "Returns the convex hull vertices in counter-clockwise order."
  [points]
  (let [pts (vec (sort-by (juxt :x :y) (distinct points)))]
    (if (<= (count pts) 1)
      (vec pts)
      (let [lower (half-hull pts)
            upper (half-hull (rseq pts))]
        (vec (concat (pop lower) (pop upper)))))))
