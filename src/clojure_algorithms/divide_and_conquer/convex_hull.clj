(ns clojure-algorithms.divide-and-conquer.convex-hull
  "Convex hull of a set of 2D points.

  Points are [x y] pairs (converted to doubles). Three algorithms:
  brute force O(n^3), divide-and-conquer O(n log n), and Melkman
  O(n log n). Each returns the hull sorted in non-decreasing order.
  Ported from TheAlgorithms/Python divide_and_conquer/convex_hull.py.")

(defn- as-point
  "Convert `p` to a [x y] pair of doubles."
  [p]
  [(double (nth p 0)) (double (nth p 1))])

(defn- validate-input
  "Validate `points` and return them as [x y] double pairs."
  [points]
  (cond
    (not (sequential? points))
    (throw (IllegalArgumentException.
            (str "Expecting an iterable object but got a non-iterable type "
                 (pr-str points))))
    (empty? points)
    (throw (IllegalArgumentException.
            (str "Expecting a list of points but got " (pr-str points))))
    :else (mapv as-point points)))

(defn det
  "Signed perpendicular distance of point `c` from segment ab.
  Positive means `c` is above ab (left), negative below (right),
  zero means collinear. 0.5 * |det| is the area of triangle abc."
  [[ax ay] [bx by] [cx cy]]
  (double (- (+ (* ax by) (* bx cy) (* cx ay))
             (+ (* ay bx) (* by cx) (* cy ax)))))

(defn- hull-edge?
  "True when segment (pts[i], pts[j]) is part of the convex hull:
  no points lie strictly on both sides and no collinear point lies
  outside the segment."
  [pts i j]
  (let [n (count pts)]
    (loop [k 0 left? false right? false]
      (cond
        (and left? right?) false
        (>= k n) true
        (or (= k i) (= k j)) (recur (inc k) left? right?)
        :else (let [d (det (pts i) (pts j) (pts k))]
                (cond
                  (> d 0) (recur (inc k) true right?)
                  (< d 0) (recur (inc k) left? true)
                  (or (neg? (compare (pts k) (pts i)))
                      (pos? (compare (pts k) (pts j))))
                  false
                  :else (recur (inc k) left? right?)))))))

(defn convex-hull-bf
  "Convex hull via the O(n^3) brute-force algorithm."
  [points]
  (let [pts (vec (sort (validate-input points)))
        n (count pts)]
    (->> (for [i (range (max 0 (dec n)))
               j (range (inc i) n)
               :when (hull-edge? pts i j)]
           [(pts i) (pts j)])
         (apply concat)
         (into (sorted-set))
         (vec))))

(defn- construct-hull
  "Add to `hull-set` the hull points among `points` above segment left-right."
  [points left right hull-set]
  (if (empty? points)
    hull-set
    (let [candidates (filterv #(pos? (det left right %)) points)]
      (if (empty? candidates)
        hull-set
        (let [extreme (reduce (fn [a b]
                                (if (> (det left right b) (det left right a))
                                  b a))
                              candidates)]
          (->> hull-set
               (construct-hull candidates left extreme)
               (construct-hull candidates extreme right)
               (#(conj % extreme))))))))

(defn convex-hull-recursive
  "Convex hull via the O(n log n) divide-and-conquer algorithm."
  [points]
  (let [pts (vec (sort (validate-input points)))
        n (count pts)
        leftmost (pts 0)
        rightmost (pts (dec n))
        middle (subvec pts 1 (max 1 (dec n)))
        upper (filterv #(pos? (det leftmost rightmost %)) middle)
        lower (filterv #(neg? (det leftmost rightmost %)) middle)]
    (->> #{leftmost rightmost}
         (construct-hull upper leftmost rightmost)
         (construct-hull lower rightmost leftmost)
         (sort)
         (vec))))

(defn convex-hull-melkman
  "Convex hull via Melkman's online algorithm, O(n log n) with sorting."
  [points]
  (let [pts (vec (sort (validate-input points)))
        n (count pts)
        [hull i] (loop [h [(pts 0) (get pts 1 (pts 0))] i 2]
                   (if (>= i n)
                     [h i]
                     (let [d (det (h 1) (h 0) (pts i))]
                       (cond
                         (> d 0) [(into [(pts i)] h) (inc i)]
                         (< d 0) [(conj h (pts i)) (inc i)]
                         :else (recur (assoc h 1 (pts i)) (inc i))))))
        hull (loop [h hull j i]
               (if (>= j n)
                 h
                 (if (and (pos? (det (h 0) (peek h) (pts j)))
                          (neg? (det (peek h) (h 0) (pts 1))))
                   (recur h (inc j))
                   (let [seed (conj (into [(pts j)] h) (pts j))
                         front (loop [v seed]
                                 (if (>= (det (v 0) (v 1) (v 2)) 0)
                                   (recur (into [(v 0)] (subvec v 2)))
                                   v))
                         both (loop [v front]
                                (let [c (count v)]
                                  (if (<= (det (peek v)
                                               (v (- c 2))
                                               (v (- c 3))) 0)
                                    (recur (conj (subvec v 0 (- c 2))
                                                 (peek v)))
                                    v)))]
                     (recur both (inc j))))))]
    (vec (sort (if (> (count hull) 3) (subvec hull 1) hull)))))
