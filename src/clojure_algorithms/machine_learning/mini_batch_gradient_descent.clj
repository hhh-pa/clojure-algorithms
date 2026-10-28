(ns clojure-algorithms.machine-learning.mini-batch-gradient-descent
  "Mini-Batch Gradient Descent : https://en.wikipedia.org/wiki/Stochastic_gradient_descent\nMini-batch gradient descent is an optimization method for training models\nby splitting the data into small batches.")

(declare mini-batch-gradient-descent)

(defn mini-batch-gradient-descent "Mini-Batch Gradient Descent for linear regression.\n\nParameters\n----------\nfeature_matrix: Feature matrix.\ntarget_values: Target values.\nlearning_rate: Learning rate.\nbatch_size: Size of mini-batches.\nn_epochs: Number of training epochs.\nrandom_seed: Random seed for reproducibility.\n\nReturns\n-------\nweights: Learned weights.\nbias: Learned bias.\n\nExample\n-------\n>>> import numpy as np\n>>> X = np.array([[1], [2], [3], [4]])\n>>> y = np.array([2, 4, 6, 8])\n>>> w, b = mini_batch_gradient_descent(\n...     X, y, learning_rate=0.1, batch_size=2, n_epochs=100, random_seed=42\n... )\n>>> round(float(w[0]), 1)  # slope close to 2\n2.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:mean,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:mean,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: mini_batch_gradient_descent stubbed (np,numpy-attr:T,numpy-attr:dot,numpy-attr:mean,numpy-attr:shape)
