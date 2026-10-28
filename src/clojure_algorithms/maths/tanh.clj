(ns clojure-algorithms.maths.tanh
  "This script demonstrates the implementation of the tangent hyperbolic\nor tanh function.\n\nThe function takes a vector of K real numbers as input and\nthen (e^x - e^(-x))/(e^x + e^(-x)). After through tanh, the\nelement of the vector mostly -1 between 1.\n\nScript inspired from its corresponding Wikipedia article\nhttps://en.wikipedia.org/wiki/Activation_function")

(declare tangent-hyperbolic)

(defn tangent-hyperbolic "    Implements the tanh function\n\n    Parameters:\n        vector: np.ndarray\n\n    Returns:\n        tanh (np.array): The input numpy array after applying tanh.\n\n    mathematically (e^x - e^(-x))/(e^x + e^(-x)) can be written as (2/(1+e^(-2x))-1\n\nExamples:\n    >>> tangent_hyperbolic(np.array([1,5,6,-0.67]))\n    array([ 0.76159416,  0.9999092 ,  0.99998771, -0.58497988])\n\n    >>> tangent_hyperbolic(np.array([8,10,2,-0.98,13]))\n    array([ 0.99999977,  1.        ,  0.96402758, -0.7530659 ,  1.        ])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: tangent_hyperbolic stubbed (np)
