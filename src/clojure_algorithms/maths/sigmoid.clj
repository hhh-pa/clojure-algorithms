(ns clojure-algorithms.maths.sigmoid
  "This script demonstrates the implementation of the Sigmoid function.\n\nThe function takes a vector of K real numbers as input and then 1 / (1 + exp(-x)).\nAfter through Sigmoid, the element of the vector mostly 0 between 1. or 1 between -1.\n\nScript inspired from its corresponding Wikipedia article\nhttps://en.wikipedia.org/wiki/Sigmoid_function")

(declare sigmoid)

(defn sigmoid "Implements the sigmoid function\n\nParameters:\n    vector (np.array): A  numpy array of shape (1,n)\n    consisting of real values\n\nReturns:\n    sigmoid_vec (np.array): The input numpy array, after applying\n    sigmoid.\n\nExamples:\n>>> sigmoid(np.array([-1.0, 1.0, 2.0]))\narray([0.26894142, 0.73105858, 0.88079708])\n\n>>> sigmoid(np.array([0.0]))\narray([0.5])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: sigmoid stubbed (np)
