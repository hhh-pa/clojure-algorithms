(ns clojure-algorithms.machine-learning.k-means-clust
  "K-Means clustering: assign points to the nearest centroid, recompute
  centroids as cluster means, repeat until assignments stabilize.

  Only the numeric core is ported; plotting (matplotlib) and the pandas
  report generator are honestly stubbed (see below).

  Author (upstream): Anurag Kumar")

(def TAG "K-MEANS-CLUST/ ")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn get-initial-centroids
  "k rows of data picked uniformly at random as initial centroids.
  A seed selects a reproducible stream."
  ([data k] (get-initial-centroids data k nil))
  ([data k seed]
   (let [rng (if (some? seed) (java.util.Random. (long seed))
                 (java.util.Random.))
         n (count data)]
     (mapv (fn [_] (nth data (.nextInt rng n))) (range k)))))

(defn- euclidean-distance
  [a b]
  (Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d))
                            a b))))

(defn centroid-pairwise-dist
  "Euclidean distance matrix between each row of x and each centroid."
  [x centroids]
  (mapv (fn [row] (mapv #(euclidean-distance row %) centroids)) x))

(defn- argmin-first
  [xs]
  (first (keep-indexed (fn [i v] (when (= v (apply min xs)) i)) xs)))

(defn assign-clusters
  "Index of the nearest centroid per data point.

  >>> (assign-clusters [[0.0 0.0] [0.0 1.0] [10.0 10.0] [10.0 11.0]]
  ...                  [[0.0 0.0] [10.0 10.0]])
  [0 0 1 1]"
  [data centroids]
  (mapv #(argmin-first %) (centroid-pairwise-dist data centroids)))

(defn revise-centroids
  "Recompute each centroid as the mean of its assigned points.
  Empty clusters yield NaN coordinates (as with the upstream mean of no rows).

  >>> (revise-centroids [[0.0 0.0] [0.0 1.0] [10.0 10.0] [10.0 11.0]]
  ...                   2 [0 0 1 1])
  [[0.0 0.5] [10.0 10.5]]"
  [data k cluster-assignment]
  (let [dims (count (first data))]
    (mapv (fn [i]
            (let [members (vec (keep-indexed
                                (fn [idx a] (when (= a i) (nth data idx)))
                                cluster-assignment))]
              (if (empty? members)
                (vec (repeat dims Double/NaN))
                (mapv (fn [dim]
                        (/ (double (reduce + (map #(nth % dim) members)))
                           (count members)))
                      (range dims)))))
          (range k))))

(defn compute-heterogeneity
  "Sum of squared distances from each point to its assigned centroid."
  [data k centroids cluster-assignment]
  (reduce + 0.0
          (map (fn [i]
                 (let [members (keep-indexed
                                (fn [idx a] (when (= a i) (nth data idx)))
                                cluster-assignment)]
                   (reduce + 0.0
                           (map #(let [d (euclidean-distance % (nth centroids i))]
                                   (* d d))
                                members))))
               (range k))))

(defn plot-heterogeneity
  "STUB: needs matplotlib (heterogeneity-over-time plot)."
  [& _args]
  (unsupported! "plt"))

(defn plot-kmeans
  "STUB: needs matplotlib (3D scatter plot)."
  [& _args]
  (unsupported! "plt"))

(defn kmeans
  "Run k-means from initial-centroids (up to maxiter iterations).
  When record-heterogeneity is an atom, each iteration's heterogeneity is
  conjed onto it. Returns [centroids cluster-assignment]."
  ([data k initial-centroids] (kmeans data k initial-centroids 500 nil false))
  ([data k initial-centroids maxiter]
   (kmeans data k initial-centroids maxiter nil false))
  ([data k initial-centroids maxiter record-heterogeneity]
   (kmeans data k initial-centroids maxiter record-heterogeneity false))
  ([data k initial-centroids maxiter record-heterogeneity verbose]
   (loop [centroids (mapv vec initial-centroids)
          prev nil
          itr 0]
     (if (>= itr maxiter)
       [centroids prev]
       (do
         (when verbose (print itr) (flush))
         (let [assignment (assign-clusters data centroids)
               centroids (revise-centroids data k assignment)]
           (if (and (some? prev) (= prev assignment))
             [centroids assignment]
             (do
               (when (and (some? prev) verbose)
                 (println (str "    " (format "%5d" (count (filter false?
                                                                   (map = prev assignment))))
                               " elements changed their cluster assignment.")))
               (when (some? record-heterogeneity)
                 (swap! record-heterogeneity conj
                        (compute-heterogeneity data k centroids assignment)))
               (recur centroids assignment (inc itr))))))))))

(defn report-generator
  "STUB: needs pandas/numpy (clustering summary DataFrame)."
  [& _args]
  (unsupported! "pd"))
