(ns clojure-algorithms.machine-learning.mean-shift
  "Mean Shift Clustering\n\nA non-parametric, centroid-based clustering algorithm that does not require\nspecifying the number of clusters in advance. It works by iteratively shifting\neach data point toward the mean of points within a given bandwidth (radius),\nuntil convergence.\n\nHow it works:\n    1. Each point starts as its own candidate centroid.\n    2. For each candidate, compute the mean of all points within `bandwidth`\n       distance (the \"window\").\n    3. Shift the candidate to that mean.\n    4. Repeat until candidates stop moving (convergence).\n    5. Merge candidates that are closer than `bandwidth` to each other.\n    6. Assign each original point to its nearest final centroid.\n\nKey Properties:\n    - No need to specify number of clusters (unlike K-Means)\n    - Can find arbitrarily shaped clusters (like DBSCAN)\n    - Sensitive to the `bandwidth` parameter\n    - Deterministic (no random initialization)\n\nTime Complexity:  O(n² * iterations) with brute-force window search\nSpace Complexity: O(n)\n\nReferences:\n    - https://en.wikipedia.org/wiki/Mean_shift\n    - Comaniciu, D. & Meer, P. \"Mean Shift: A Robust Approach Toward\n      Feature Space Analysis.\" IEEE TPAMI, 2002.\n      https://doi.org/10.1109/34.1000236"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare euclidean-distance get-points-within-bandwidth compute-mean shift-point has-converged merge-centroids mean-shift)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice py-sub py-div py-pow py-min py-min-by py-max py-max-by py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-slice [c lo hi step]
  (when (map? c) (throw (ex-info "unhashable slice" {:type :TypeError})))
  (let [s (vec (seq c)) n (count s)
        step (if (nil? step) 1 step)]
    (when (zero? step) (throw (ex-info "slice step cannot be zero" {:type :ValueError})))
    (let [idxs (if (pos? step)
                 (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                        (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                        step)
                 (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                        (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                        step))
          res (mapv s idxs)]
      (if (string? c) (apply str res) res))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defn euclidean-distance "Compute the Euclidean distance between two n-dimensional points.\n\n>>> euclidean_distance([0.0, 0.0], [3.0, 4.0])\n5.0\n>>> euclidean_distance([1.0, 1.0], [1.0, 1.0])\n0.0\n>>> euclidean_distance([0.0], [5.0])\n5.0\n>>> euclidean_distance([0.0, 0.0], [1.0])\nTraceback (most recent call last):\n    ...\nValueError: Both points must have the same number of dimensions."
  [point-a point-b] (try (when (py-truthy (not (py-eq (count point-a) (count point-b)))) (throw (ex-info (py-str "Both points must have the same number of dimensions.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow (reduce +' 0 (doall (for [[a b] (py-iter (map vector point-a point-b))] (py-pow (py-sub a b) 2)))) 0.5)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn get-points-within-bandwidth "Return all points in data that lie within `bandwidth` distance of `center`.\n\n>>> data = [[0.0, 0.0], [0.5, 0.5], [5.0, 5.0]]\n>>> get_points_within_bandwidth(data, [0.0, 0.0], 1.0)\n[[0.0, 0.0], [0.5, 0.5]]\n>>> get_points_within_bandwidth(data, [5.0, 5.0], 1.0)\n[[5.0, 5.0]]\n>>> get_points_within_bandwidth(data, [0.0, 0.0], 10.0)\n[[0.0, 0.0], [0.5, 0.5], [5.0, 5.0]]"
  [data center bandwidth] (vec (for [point (py-iter data) :when (py-truthy (py-le (euclidean-distance point center) bandwidth))] point)))

(defn compute-mean "Compute the element-wise mean of a list of points.\n\n>>> compute_mean([[1.0, 2.0], [3.0, 4.0]])\n[2.0, 3.0]\n>>> compute_mean([[0.0, 0.0, 0.0]])\n[0.0, 0.0, 0.0]\n>>> compute_mean([])\nTraceback (most recent call last):\n    ...\nValueError: Cannot compute mean of empty list."
  [points] (let [n-dims (atom nil)] (try (when (py-truthy (not (py-truthy points))) (throw (ex-info (py-str "Cannot compute mean of empty list.") {:type :ValueError}))) (reset! n-dims (count (py-get points 0))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [dim (py-iter (range @n-dims))] (py-div (reduce +' 0 (doall (for [point (py-iter points)] (py-get point dim)))) (count points))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn shift-point "Shift a single point to the mean of all data points within `bandwidth`.\n\nIf no points fall within the bandwidth, the point remains unchanged.\n\n>>> data = [[1.0, 1.0], [1.5, 1.5], [10.0, 10.0]]\n>>> shift_point([1.0, 1.0], data, 2.0)\n[1.25, 1.25]\n>>> shift_point([10.0, 10.0], data, 1.0)\n[10.0, 10.0]"
  [point data bandwidth] (let [neighbors (atom nil)] (try (reset! neighbors (get-points-within-bandwidth data point bandwidth)) (when (py-truthy (not (py-truthy @neighbors))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value point}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (compute-mean @neighbors)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn has-converged "Check whether a point has converged (moved less than `tolerance`).\n\n>>> has_converged([1.0, 1.0], [1.0000001, 1.0000001], 1e-4)\nTrue\n>>> has_converged([1.0, 1.0], [1.5, 1.5], 1e-4)\nFalse"
  [old-point new-point tolerance] (py-lt (euclidean-distance old-point new-point) tolerance))

(defn merge-centroids "Merge centroids that are within `bandwidth` distance of each other.\n\nIterates through centroids and greedily merges any that are close enough,\nkeeping the first encountered as the representative.\n\n>>> centroids = [[1.0, 1.0], [1.1, 1.1], [10.0, 10.0]]\n>>> merged = merge_centroids(centroids, 1.0)\n>>> len(merged)\n2\n>>> centroids = [[0.0, 0.0], [5.0, 5.0], [10.0, 10.0]]\n>>> len(merge_centroids(centroids, 1.0))\n3"
  [centroids bandwidth] (let [centroid (atom nil) merged (atom nil)] (try (reset! merged []) (doseq [__i1 (py-iter centroids)] (reset! centroid __i1) (when (py-truthy (every? py-truthy (doall (for [existing (py-iter @merged)] (py-ge (euclidean-distance @centroid existing) bandwidth))))) (swap! merged conj @centroid))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @merged})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn mean-shift "Perform Mean Shift clustering on a dataset.\n\nArgs:\n    data:           List of n-dimensional data points.\n    bandwidth:      Radius of the window used to compute the mean.\n                    Must be greater than 0.\n    max_iterations: Maximum number of shift iterations per point.\n                    Must be at least 1.\n    tolerance:      Convergence threshold — stop shifting when movement\n                    is smaller than this value. Must be greater than 0.\n\nReturns:\n    A list of integer cluster labels, one per input point.\n    Cluster IDs start from 0.\n\nRaises:\n    ValueError: If data is empty.\n    ValueError: If bandwidth is not positive.\n    ValueError: If max_iterations is less than 1.\n    ValueError: If tolerance is not positive.\n\nExample — two well-separated clusters:\n>>> data = [\n...     [1.0, 1.0], [1.2, 1.0], [1.0, 1.2],\n...     [9.0, 9.0], [9.2, 9.0], [9.0, 9.2],\n... ]\n>>> labels = mean_shift(data, bandwidth=2.0)\n>>> len(set(labels))  # two clusters\n2\n>>> labels[0] == labels[1] == labels[2]  # first group same cluster\nTrue\n>>> labels[3] == labels[4] == labels[5]  # second group same cluster\nTrue\n>>> labels[0] != labels[3]               # different clusters\nTrue\n\nExample — single cluster (all points close together):\n>>> data = [[0.0, 0.0], [0.1, 0.0], [0.0, 0.1], [0.1, 0.1]]\n>>> labels = mean_shift(data, bandwidth=2.0)\n>>> len(set(labels))\n1\n\nExample — invalid inputs:\n>>> mean_shift([], bandwidth=1.0)\nTraceback (most recent call last):\n    ...\nValueError: Data must not be empty.\n>>> mean_shift([[1.0, 2.0]], bandwidth=0.0)\nTraceback (most recent call last):\n    ...\nValueError: Bandwidth must be greater than 0.\n>>> mean_shift([[1.0, 2.0]], bandwidth=1.0, max_iterations=0)\nTraceback (most recent call last):\n    ...\nValueError: max_iterations must be at least 1.\n>>> mean_shift([[1.0, 2.0]], bandwidth=1.0, tolerance=0.0)\nTraceback (most recent call last):\n    ...\nValueError: Tolerance must be greater than 0."
  ([data bandwidth] (mean-shift data bandwidth 300 0.0001))
  ([data bandwidth max-iterations] (mean-shift data bandwidth max-iterations 0.0001))
  ([data bandwidth max-iterations tolerance] (let [--v (atom nil) candidates (atom nil) centroids (atom nil) labels (atom nil) new-candidates (atom nil)] (try (when (py-truthy (not (py-truthy data))) (throw (ex-info (py-str "Data must not be empty.") {:type :ValueError}))) (when (py-truthy (py-le bandwidth 0)) (throw (ex-info (py-str "Bandwidth must be greater than 0.") {:type :ValueError}))) (when (py-truthy (py-lt max-iterations 1)) (throw (ex-info (py-str "max_iterations must be at least 1.") {:type :ValueError}))) (when (py-truthy (py-le tolerance 0)) (throw (ex-info (py-str "Tolerance must be greater than 0.") {:type :ValueError}))) (reset! candidates (vec (for [point (py-iter data)] (py-slice point nil nil nil)))) (try (doseq [__i1 (py-iter (range max-iterations))] (reset! --v __i1) (reset! new-candidates (vec (for [candidate (py-iter @candidates)] (shift-point candidate data bandwidth)))) (when (py-truthy (every? py-truthy (doall (for [[old new-v] (py-iter (map vector @candidates @new-candidates))] (has-converged old new-v tolerance))))) (throw (ex-info "__break__" {:__control__ true :__break__ true}))) (reset! candidates @new-candidates)) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (reset! centroids (merge-centroids @candidates bandwidth)) (reset! labels (vec (for [point (py-iter data)] (py-min-by (fn [centroid-index] (euclidean-distance point (py-get @centroids centroid-index))) (range (count @centroids)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @labels})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
