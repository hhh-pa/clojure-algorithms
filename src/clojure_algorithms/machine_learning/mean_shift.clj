(ns clojure-algorithms.machine-learning.mean-shift
  "Mean Shift clustering.

  A non-parametric, centroid-based algorithm: each point is iteratively shifted
  toward the mean of points within a bandwidth window until convergence, then
  nearby candidates are merged and every point is assigned to its nearest
  centroid. The cluster count emerges from the data.

  References:
  https://en.wikipedia.org/wiki/Mean_shift
  Comaniciu, D. & Meer, P. \"Mean Shift: A Robust Approach Toward Feature
  Space Analysis.\" IEEE TPAMI, 2002.")

(defn euclidean-distance
  "Euclidean distance between two n-dimensional points.
  Throws :ValueError when the points have different dimensions."
  [point-a point-b]
  (when (not= (count point-a) (count point-b))
    (throw (ex-info "Both points must have the same number of dimensions."
                    {:type :ValueError})))
  (Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d))
                            point-a point-b))))

(defn get-points-within-bandwidth
  "All points of data lying within bandwidth distance of center."
  [data center bandwidth]
  (vec (filter #(<= (euclidean-distance % center) bandwidth) data)))

(defn compute-mean
  "Element-wise mean of points. Throws :ValueError on empty input."
  [points]
  (when (empty? points)
    (throw (ex-info "Cannot compute mean of empty list." {:type :ValueError})))
  (let [n (count points)
        dims (count (first points))]
    (mapv (fn [dim]
            (/ (double (reduce + (map #(nth % dim) points))) n))
          (range dims))))

(defn shift-point
  "Shift point to the mean of data points within bandwidth of it.
  The point is unchanged when the window is empty."
  [point data bandwidth]
  (let [neighbors (get-points-within-bandwidth data point bandwidth)]
    (if (empty? neighbors) point (compute-mean neighbors))))

(defn has-converged
  "True when old-point moved less than tolerance to reach new-point."
  [old-point new-point tolerance]
  (< (euclidean-distance old-point new-point) tolerance))

(defn merge-centroids
  "Greedily drop centroids within bandwidth of an already kept one."
  [centroids bandwidth]
  (reduce (fn [merged centroid]
            (if (every? #(>= (euclidean-distance centroid %) bandwidth) merged)
              (conj merged centroid)
              merged))
          []
          centroids))

(defn mean-shift
  "Cluster data with Mean Shift. Returns one integer label per point, with
  cluster ids starting at 0.

  Throws :ValueError when data is empty, bandwidth/tolerance are not
  positive, or max-iterations is less than 1."
  ([data bandwidth] (mean-shift data bandwidth 300 1e-4))
  ([data bandwidth max-iterations] (mean-shift data bandwidth max-iterations 1e-4))
  ([data bandwidth max-iterations tolerance]
   (when (empty? data)
     (throw (ex-info "Data must not be empty." {:type :ValueError})))
   (when (<= bandwidth 0)
     (throw (ex-info "Bandwidth must be greater than 0." {:type :ValueError})))
   (when (< max-iterations 1)
     (throw (ex-info "max_iterations must be at least 1." {:type :ValueError})))
   (when (<= tolerance 0)
     (throw (ex-info "Tolerance must be greater than 0." {:type :ValueError})))
   (let [candidates
         (loop [candidates (vec data)
                iteration 0]
           (if (>= iteration max-iterations)
             candidates
             (let [shifted (mapv #(shift-point % data bandwidth) candidates)]
               (if (every? true? (map #(has-converged %1 %2 tolerance)
                                      candidates shifted))
                 shifted
                 (recur shifted (inc iteration))))))
         centroids (merge-centroids candidates bandwidth)]
     (mapv (fn [point]
             (apply min-key #(euclidean-distance point (nth centroids %))
                    (range (count centroids))))
           data))))
