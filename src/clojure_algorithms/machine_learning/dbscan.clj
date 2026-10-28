(ns clojure-algorithms.machine-learning.dbscan
  "DBSCAN (Density-Based Spatial Clustering of Applications with Noise)\n\nA density-based clustering algorithm that groups together points that are\nclosely packed together, while marking points in low-density regions as outliers.\n\nUnlike K-Means, DBSCAN:\n- Does NOT require specifying the number of clusters in advance\n- Can find clusters of arbitrary shapes\n- Is robust to outliers (labels them as noise, cluster id = -1)\n\nKey Parameters:\n    epsilon (eps): The maximum distance between two points to be considered neighbors\n    min_points: Minimum number of points to form a dense region (core point)\n\nPoint Types:\n    - Core point:    Has at least `min_points` neighbors within `epsilon` distance\n    - Border point:  Within `epsilon` of a core point, but has fewer than\n                     `min_points` neighbors\n    - Noise point:   Neither core nor border — labeled as -1\n\nTime Complexity:  O(n²) with brute-force neighbor search\nSpace Complexity: O(n)\n\nReferences:\n    - https://en.wikipedia.org/wiki/DBSCAN\n    - Ester, M., et al. \"A density-based algorithm for discovering clusters.\"\n      KDD 1996. https://dl.acm.org/doi/10.5555/3001460.3001507"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare euclidean-distance get-neighbors dbscan)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-pow py-neg py-pop2 py-float-str py-quote py-repr py-str)

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

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

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

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

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

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-pop2
  ([c k] (py-pop2 c k :py-no-default))
  ([c k dflt]
   (cond (map? c) (if (contains? c k) [(get c k) (dissoc c k)]
                      (if (= dflt :py-no-default)
                        (throw (ex-info (str "key: " k) {:type :KeyError})) [dflt c]))
         (set? c) (if (nil? k)
                    (if (empty? c)
                      (throw (ex-info "pop from empty set" {:type :KeyError}))
                      [(first c) (disj c (first c))])
                    (if (contains? c k) [k (disj c k)]
                        (throw (ex-info (str "key: " k) {:type :KeyError}))))
         :else (let [v (vec c)]
                 (if (nil? k)
                   (if (empty? v)
                     (throw (ex-info "pop from empty list" {:type :IndexError}))
                     [(peek v) (pop v)])
                   (let [i (py-index-norm k (count v))]
                     [(nth v i) (vec (concat (subvec v 0 i) (subvec v (inc i))))]))))))

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

(defn euclidean-distance "Compute the Euclidean distance between two points in n-dimensional space.\n\n>>> euclidean_distance([0.0, 0.0], [3.0, 4.0])\n5.0\n>>> euclidean_distance([1.0, 2.0, 3.0], [1.0, 2.0, 3.0])\n0.0\n>>> euclidean_distance([0.0], [5.0])\n5.0\n>>> euclidean_distance([0.0, 0.0], [1.0])\nTraceback (most recent call last):\n    ...\nValueError: Both points must have the same number of dimensions."
  [point-a point-b] (try (when (py-truthy (not (py-eq (count point-a) (count point-b)))) (throw (ex-info (py-str "Both points must have the same number of dimensions.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-pow (reduce +' 0 (doall (for [[a b] (py-iter (map vector point-a point-b))] (py-pow (py-sub a b) 2)))) 0.5)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn get-neighbors "Return indices of all points within epsilon distance of data[point_index].\n\n>>> data = [[0.0, 0.0], [0.1, 0.1], [5.0, 5.0]]\n>>> get_neighbors(data, 0, 0.5)\n[0, 1]\n>>> get_neighbors(data, 2, 0.5)\n[2]\n>>> get_neighbors(data, 0, 10.0)\n[0, 1, 2]"
  [data point-index epsilon] (vec (for [[index point] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) data)) :when (py-truthy (py-le (euclidean-distance (py-get data point-index) point) epsilon))] index)))

(defn dbscan "Perform DBSCAN clustering on a dataset.\n\nArgs:\n    data:       List of n-dimensional data points, e.g. [[x1,y1], [x2,y2], ...]\n    epsilon:    Maximum distance between two points to be considered neighbors.\n                Must be greater than 0.\n    min_points: Minimum number of neighbors (including self) to be a core point.\n                Must be at least 1.\n\nReturns:\n    A list of integer cluster labels, one per input point.\n    Noise points are labeled -1.\n    Cluster IDs start from 0.\n\nRaises:\n    ValueError: If data is empty.\n    ValueError: If epsilon is not positive.\n    ValueError: If min_points is less than 1.\n\nExample — two well-separated clusters:\n>>> data = [\n...     [1.0, 1.0], [1.1, 1.0], [1.0, 1.1],\n...     [9.0, 9.0], [9.1, 9.0], [9.0, 9.1],\n... ]\n>>> labels = dbscan(data, epsilon=0.5, min_points=2)\n>>> len(set(labels))  # two clusters\n2\n>>> labels[0] == labels[1] == labels[2]  # first three in same cluster\nTrue\n>>> labels[3] == labels[4] == labels[5]  # last three in same cluster\nTrue\n>>> labels[0] != labels[3]               # different clusters\nTrue\n\nExample — isolated noise point:\n>>> data = [[0.0, 0.0], [0.1, 0.0], [0.0, 0.1], [99.0, 99.0]]\n>>> labels = dbscan(data, epsilon=0.5, min_points=2)\n>>> labels[3]  # noise\n-1\n>>> labels[0] == labels[1] == labels[2]  # one cluster\nTrue\n\nExample — all points are noise (min_points too high):\n>>> data = [[0.0, 0.0], [5.0, 5.0]]\n>>> dbscan(data, epsilon=0.3, min_points=5)\n[-1, -1]\n\nExample — single cluster (all points close together):\n>>> data = [[0.0, 0.0], [0.1, 0.0], [0.0, 0.1], [0.1, 0.1]]\n>>> labels = dbscan(data, epsilon=0.5, min_points=2)\n>>> len(set(labels))\n1\n>>> -1 not in labels\nTrue\n\nExample — invalid inputs:\n>>> dbscan([], epsilon=0.5, min_points=2)\nTraceback (most recent call last):\n    ...\nValueError: Data must not be empty.\n>>> dbscan([[1.0, 2.0]], epsilon=0.0, min_points=2)\nTraceback (most recent call last):\n    ...\nValueError: Epsilon must be greater than 0.\n>>> dbscan([[1.0, 2.0]], epsilon=0.5, min_points=0)\nTraceback (most recent call last):\n    ...\nValueError: min_points must be at least 1."
  [data epsilon min-points] (let [current-cluster-id (atom nil) current-neighbors (atom nil) current-point (atom nil) labels (atom nil) neighbor (atom nil) neighbors (atom nil) point-index (atom nil) seeds (atom nil)] (try (when (py-truthy (not (py-truthy data))) (throw (ex-info (py-str "Data must not be empty.") {:type :ValueError}))) (when (py-truthy (py-le epsilon 0)) (throw (ex-info (py-str "Epsilon must be greater than 0.") {:type :ValueError}))) (when (py-truthy (py-lt min-points 1)) (throw (ex-info (py-str "min_points must be at least 1.") {:type :ValueError}))) (reset! labels (py-mul [(py-neg 1)] (count data))) (reset! current-cluster-id 0) (doseq [__i1 (py-iter (range (count data)))] (try (reset! point-index __i1) (when (py-truthy (not (py-eq (py-get @labels @point-index) (py-neg 1)))) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (reset! neighbors (get-neighbors data @point-index epsilon)) (when (py-truthy (py-lt (count @neighbors) min-points)) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (swap! labels (fn [__c] (py-assoc-in __c [@point-index] @current-cluster-id))) (reset! seeds (vec (for [n (py-iter @neighbors) :when (py-truthy (not (py-eq n @point-index)))] n))) (while (py-truthy @seeds) (try (let [__p2 (py-pop2 @seeds nil)] (reset! seeds (second __p2)) (reset! current-point (first __p2))) (when (py-truthy (let [__b3 (not (py-eq (py-get @labels @current-point) (py-neg 1)))] (if (py-truthy __b3) (not (py-eq (py-get @labels @current-point) @current-cluster-id)) __b3))) (throw (ex-info "__continue__" {:__control__ true :__continue__ true}))) (swap! labels (fn [__c] (py-assoc-in __c [@current-point] @current-cluster-id))) (reset! current-neighbors (get-neighbors data @current-point epsilon)) (when (py-truthy (py-ge (count @current-neighbors) min-points)) (doseq [__i4 (py-iter @current-neighbors)] (reset! neighbor __i4) (when (py-truthy (py-eq (py-get @labels @neighbor) (py-neg 1))) (swap! seeds conj @neighbor)))) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e))))) (swap! current-cluster-id py-add 1) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @labels})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
