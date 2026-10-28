(ns clojure-algorithms.machine-learning.linear-regression-vectorized
  "Vectorized Linear Regression using Gradient Descent\n\nAuthor: Somrita Banerjee (mailto:somritabanerjee126@gmail.com)\n\nDataset used: CSGO dataset (ADR vs Rating)\n\nReferences:\nhttps://en.wikipedia.org/wiki/Linear_regression")

(declare collect-dataset gradient-descent mean-absolute-error main)

(defn collect-dataset "Collect dataset of CSGO (ADR vs Rating).\n\n:return: dataset as a NumPy array\n\n>>> ds = collect_dataset()\n>>> isinstance(ds, np.ndarray)\nTrue\n>>> ds.shape[1] >= 2\nTrue"
  [& _args]
  ;; stub: original requires httpx2,np; pure-Clojure port not applicable
  (throw (ex-info "requires httpx2,np" {:type :UnsupportedOperation})))

(defn gradient-descent "Run gradient descent in a fully vectorized form.\n\n:param features: dataset features\n:param labels: dataset labels\n:param alpha: learning rate\n:param iterations: number of iterations\n:return: learned feature vector theta\n\n>>> import numpy as np\n>>> features = np.array([[1, 1], [1, 2], [1, 3]])\n>>> labels = np.array([[1], [2], [3]])\n>>> theta = gradient_descent(\n...     features, labels, alpha=0.01, iterations=1000  # doctest: +SKIP\n... )"
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn mean-absolute-error "Return mean absolute error.\n\n>>> pred = np.array([3, -0.5, 2, 7])\n>>> orig = np.array([2.5, 0.0, 2, 8])\n>>> mean_absolute_error(pred, orig)\n0.5"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn main
  [& _args]
  ;; stub: original requires np,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: collect_dataset stubbed (httpx2,np)
;; NOTE: gradient_descent stubbed (np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum)
;; NOTE: mean_absolute_error stubbed (np,numpy-attr:mean)
;; NOTE: main stubbed (np,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape)
