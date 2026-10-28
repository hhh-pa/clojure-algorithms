(ns clojure-algorithms.geometry.graham-scan
  "Convex hull via the Graham scan algorithm.")

(defrecord Point [x y])

(defn euclidean-distance
  "Euclidean distance between points a and b."
  [a b]
  (Math/sqrt (+ (* (- (:x a) (:x b)) (- (:x a) (:x b)))
                (* (- (:y a) (:y b)) (- (:y a) (:y b))))))

(defn consecutive-orientation
  "Cross product of (self->a) and (a->b). Positive means
   counter-clockwise, negative clockwise, zero collinear."
  [self a b]
  (- (* (- (:x a) (:x self)) (- (:y b) (:y a)))
     (* (- (:y a) (:y self)) (- (:x b) (:x a)))))

(defn graham-scan
  "Returns the convex hull in counter-clockwise order, or [] when fewer
   than 3 distinct points are given or all points are collinear."
  [points]
  (if (<= (count points) 2)
    []
    (let [pivot (first (sort-by (juxt :y :x) points))
          others (vec (remove #(= % pivot) points))]
      (if (empty? others)
        []
        (let [cmp (fn [a b]
                    (let [o (consecutive-orientation pivot a b)]
                      (cond (neg? o) 1
                            (pos? o) -1
                            :else (let [da (euclidean-distance pivot a)
                                        db (euclidean-distance pivot b)]
                                    (cond (< db da) -1 (> db da) 1 :else 0)))))
              sorted-pts (sort cmp others)
              hull (reduce
                    (fn [hull pt]
                      (if (zero? (consecutive-orientation pivot pt (peek hull)))
                        hull
                        (loop [h hull]
                          (if (and (>= (count h) 2)
                                   (<= (consecutive-orientation
                                        (nth h (- (count h) 2)) (peek h) pt) 0))
                            (recur (pop h))
                            (conj h pt)))))
                    [pivot (first sorted-pts)] (rest sorted-pts))]
          (if (<= (count hull) 2) [] (vec hull)))))))
