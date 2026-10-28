(ns clojure-algorithms.machine-learning.k-nearest-neighbors
  "k-Nearest Neighbors (kNN) is a simple non-parametric supervised learning\nalgorithm used for classification. Given some labelled training data, a given\npoint is classified using its k nearest neighbours according to some distance\nmetric. The most commonly occurring label among the neighbours becomes the label\nof the given point. In effect, the label of the given point is decided by a\nmajority vote.\n\nThis implementation uses the commonly used Euclidean distance metric, but other\ndistance metrics can also be used.\n\nReference: https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm")

(declare make-KNN KNN-euclidean-distance KNN-classify)

(defrecord KNN [data labels])

(defn make-KNN "Constructor for KNN."
  [& _args]
  ;; stub: original requires NDArray,np; pure-Clojure port not applicable
  (throw (ex-info "requires NDArray,np" {:type :UnsupportedOperation})))

(defn KNN-euclidean-distance "Calculate the Euclidean distance between two points\n>>> KNN._euclidean_distance(np.array([0, 0]), np.array([3, 4]))\n5.0\n>>> KNN._euclidean_distance(np.array([1, 2, 3]), np.array([1, 8, 11]))\n10.0"
  [& _args]
  ;; stub: original requires NDArray,np; pure-Clojure port not applicable
  (throw (ex-info "requires NDArray,np" {:type :UnsupportedOperation})))

(defn KNN-classify "Classify a given point using the kNN algorithm\n>>> train_X = np.array(\n...     [[0, 0], [1, 0], [0, 1], [0.5, 0.5], [3, 3], [2, 3], [3, 2]]\n... )\n>>> train_y = np.array([0, 0, 0, 0, 1, 1, 1])\n>>> classes = ['A', 'B']\n>>> knn = KNN(train_X, train_y, classes)\n>>> point = np.array([1.2, 1.2])\n>>> knn.classify(point)\n'A'"
  [& _args]
  ;; stub: original requires NDArray,np; pure-Clojure port not applicable
  (throw (ex-info "requires NDArray,np" {:type :UnsupportedOperation})))

;; NOTE: KNN __init__ stubbed (NDArray,np)
;; NOTE: KNN._euclidean_distance stubbed (NDArray,np)
;; NOTE: KNN.classify stubbed (NDArray,np)
