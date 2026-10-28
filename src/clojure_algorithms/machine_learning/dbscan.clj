(ns clojure-algorithms.machine-learning.dbscan
  "DBSCAN (Density-Based Spatial Clustering of Applications with Noise).

  Groups together closely packed points while marking low-density points as
  noise (label -1). Unlike K-Means it needs no advance cluster count, finds
  arbitrarily shaped clusters and is robust to outliers.

  References:
  https://en.wikipedia.org/wiki/DBSCAN
  Ester, M., et al. \"A density-based algorithm for discovering clusters.\"
  KDD 1996.")

(defn euclidean-distance
  "Euclidean distance between two n-dimensional points.
  Throws :ValueError when the points have different dimensions."
  [point-a point-b]
  (when (not= (count point-a) (count point-b))
    (throw (ex-info "Both points must have the same number of dimensions."
                    {:type :ValueError})))
  (Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d))
                            point-a point-b))))

(defn get-neighbors
  "Indices of all points within epsilon distance of (nth data point-index),
  including point-index itself."
  [data point-index epsilon]
  (let [origin (nth data point-index)]
    (vec (keep-indexed (fn [i point]
                         (when (<= (euclidean-distance origin point) epsilon) i))
                       data))))

(defn- expand-cluster
  "Grow the cluster started at point-index; returns updated labels."
  [data labels point-index neighbors cluster-id epsilon min-points]
  (loop [labels (assoc labels point-index cluster-id)
         seeds (vec (remove #(= % point-index) neighbors))]
    (if (empty? seeds)
      labels
      (let [current (peek seeds)
            seeds (pop seeds)
            label (nth labels current)]
        (if (and (not= label -1) (not= label cluster-id))
          (recur labels seeds)
          (let [labels (assoc labels current cluster-id)
                current-neighbors (get-neighbors data current epsilon)]
            (recur labels
                   (if (>= (count current-neighbors) min-points)
                     (into seeds (filter #(= (nth labels %) -1)
                                         current-neighbors))
                     seeds))))))))

(defn dbscan
  "Cluster data with DBSCAN. Returns one integer label per point; noise is -1
  and cluster ids start at 0.

  Throws :ValueError when data is empty, epsilon is not positive, or
  min-points is less than 1."
  [data epsilon min-points]
  (when (empty? data)
    (throw (ex-info "Data must not be empty." {:type :ValueError})))
  (when (<= epsilon 0)
    (throw (ex-info "Epsilon must be greater than 0." {:type :ValueError})))
  (when (< min-points 1)
    (throw (ex-info "min_points must be at least 1." {:type :ValueError})))
  (loop [labels (vec (repeat (count data) -1))
         cluster-id 0
         point-index 0]
    (if (>= point-index (count data))
      labels
      (let [neighbors (get-neighbors data point-index epsilon)]
        (if (or (not= (nth labels point-index) -1)
                (< (count neighbors) min-points))
          (recur labels cluster-id (inc point-index))
          (recur (expand-cluster data labels point-index neighbors cluster-id
                                 epsilon min-points)
                 (inc cluster-id)
                 (inc point-index)))))))
