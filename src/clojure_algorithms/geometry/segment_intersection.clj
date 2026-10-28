(ns clojure-algorithms.geometry.segment-intersection
  "Line-segment intersection via the CLRS orientation method.")

(defrecord Point [x y])

(defn direction
  "Cross product of (pivot->query) and (pivot->target). Negative means
   counter-clockwise, positive clockwise, zero collinear."
  [pivot target query]
  (- (* (- (:x query) (:x pivot)) (- (:y target) (:y pivot)))
     (* (- (:x target) (:x pivot)) (- (:y query) (:y pivot)))))

(defn on-segment?
  "True when collinear point lies within the bounding box of the segment."
  [seg-start seg-end point]
  (and (<= (min (:x seg-start) (:x seg-end)) (:x point)
           (max (:x seg-start) (:x seg-end)))
       (<= (min (:y seg-start) (:y seg-end)) (:y point)
           (max (:y seg-start) (:y seg-end)))))

(defn segments-intersect?
  "True when segment p1p2 intersects segment p3p4."
  [p1 p2 p3 p4]
  (let [d1 (direction p3 p4 p1)
        d2 (direction p3 p4 p2)
        d3 (direction p1 p2 p3)
        d4 (direction p1 p2 p4)
        straddles? (fn [a b] (or (< a 0 b) (< b 0 a)))]
    (cond
      (and (straddles? d1 d2) (straddles? d3 d4)) true
      (and (zero? d1) (on-segment? p3 p4 p1)) true
      (and (zero? d2) (on-segment? p3 p4 p2)) true
      (and (zero? d3) (on-segment? p1 p2 p3)) true
      :else (and (zero? d4) (on-segment? p1 p2 p4)))))
