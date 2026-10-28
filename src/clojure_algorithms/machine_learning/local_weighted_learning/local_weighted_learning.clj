(ns clojure-algorithms.machine-learning.local-weighted-learning.local-weighted-learning
  "Locally weighted linear regression, also called local regression, is a type of\nnon-parametric linear regression that prioritizes data closest to a given\nprediction point. The algorithm estimates the vector of model coefficients β\nusing weighted least squares regression:\n\nβ = (XᵀWX)⁻¹(XᵀWy),\n\nwhere X is the design matrix, y is the response vector, and W is the diagonal\nweight matrix.\n\nThis implementation calculates wᵢ, the weight of the ith training sample, using\nthe Gaussian weight:\n\nwᵢ = exp(-‖xᵢ - x‖²/(2τ²)),\n\nwhere xᵢ is the ith training sample, x is the prediction point, τ is the\n\"bandwidth\", and ‖x‖ is the Euclidean norm (also called the 2-norm or the L²\nnorm). The bandwidth τ controls how quickly the weight of a training sample\ndecreases as its distance from the prediction point increases. One can think of\nthe Gaussian weight as a bell curve centered around the prediction point: a\ntraining sample is weighted lower if it's farther from the center, and τ\ncontrols the spread of the bell curve.\n\nOther types of locally weighted regression such as locally estimated scatterplot\nsmoothing (LOESS) typically use different weight functions.\n\nReferences:\n    - https://en.wikipedia.org/wiki/Local_regression\n    - https://en.wikipedia.org/wiki/Weighted_least_squares\n    - https://cs229.stanford.edu/notes2022fall/main_notes.pdf")

(declare weight-matrix local-weight local-weight-regression load-data plot-preds)

(defn weight-matrix "Calculate the weight of every point in the training data around a given\nprediction point\n\nArgs:\n    point: x-value at which the prediction is being made\n    x_train: ndarray of x-values for training\n    tau: bandwidth value, controls how quickly the weight of training values\n        decreases as the distance from the prediction point increases\n\nReturns:\n    m x m weight matrix around the prediction point, where m is the size of\n    the training set\n>>> weight_matrix(\n...     np.array([16.99, 10.34]),\n...     np.array([[16.99, 10.34], [21.01, 23.68], [24.59, 25.69]]),\n...     5,\n... ).round(4)\narray([[1.    , 0.    , 0.    ],\n       [0.    , 0.0206, 0.    ],\n       [0.    , 0.    , 0.0028]])"
  [& _args]
  ;; stub: original requires np,numpy-attr:T; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T" {:type :UnsupportedOperation})))

(defn local-weight "Calculate the local weights at a given prediction point using the weight\nmatrix for that point\n\nArgs:\n    point: x-value at which the prediction is being made\n    x_train: ndarray of x-values for training\n    y_train: ndarray of y-values for training\n    tau: bandwidth value, controls how quickly the weight of training values\n        decreases as the distance from the prediction point increases\nReturns:\n    ndarray of local weights\n>>> local_weight(\n...     np.array([16.99, 10.34]),\n...     np.array([[16.99, 10.34], [21.01, 23.68], [24.59, 25.69]]),\n...     np.array([[1.01, 1.66, 3.5]]),\n...     5,\n... ).round(5)\narray([[0.02572],\n       [0.05552]])"
  [& _args]
  ;; stub: original requires np,numpy-attr:T; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T" {:type :UnsupportedOperation})))

(defn local-weight-regression "Calculate predictions for each point in the training data\n\nArgs:\n    x_train: ndarray of x-values for training\n    y_train: ndarray of y-values for training\n    tau: bandwidth value, controls how quickly the weight of training values\n        decreases as the distance from the prediction point increases\n\nReturns:\n    ndarray of predictions\n>>> local_weight_regression(\n...     np.array([[16.99, 10.34], [21.01, 23.68], [24.59, 25.69]]),\n...     np.array([[1.01, 1.66, 3.5]]),\n...     5,\n... ).round(5)\narray([1.01094, 1.98589, 3.42233])"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot,numpy-attr:item; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot,numpy-attr:item" {:type :UnsupportedOperation})))

(defn load-data "Load data from seaborn and split it into x and y points\n>>> pass    # No doctests, function is for demo purposes only"
  [& _args]
  ;; stub: original requires np,sns; pure-Clojure port not applicable
  (throw (ex-info "requires np,sns" {:type :UnsupportedOperation})))

(defn plot-preds "Plot predictions and display the graph\n>>> pass    # No doctests, function is for demo purposes only"
  [& _args]
  ;; stub: original requires np,plt; pure-Clojure port not applicable
  (throw (ex-info "requires np,plt" {:type :UnsupportedOperation})))

;; NOTE: weight_matrix stubbed (np,numpy-attr:T)
;; NOTE: local_weight stubbed (np,numpy-attr:T)
;; NOTE: local_weight_regression stubbed (np,numpy-attr:dot,numpy-attr:item)
;; NOTE: load_data stubbed (np,sns)
;; NOTE: plot_preds stubbed (np,plt)
