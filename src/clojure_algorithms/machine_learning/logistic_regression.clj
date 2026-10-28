(ns clojure-algorithms.machine-learning.logistic-regression
  "Implementing logistic regression for classification problem\nHelpful resources:\nCoursera ML course\nhttps://medium.com/@martinpella/logistic-regression-from-scratch-in-python-124c5636b8ac")

(declare sigmoid-function cost-function log-likelihood logistic-reg)

(defn sigmoid-function "Also known as Logistic Function.\n\n            1\nf(x) =   -------\n          1 + e⁻ˣ\n\nThe sigmoid function approaches a value of 1 as its input 'x' becomes\nincreasing positive. Opposite for negative values.\n\nReference: https://en.wikipedia.org/wiki/Sigmoid_function\n\n@param z:  input to the function\n@returns: returns value in the range 0 to 1\n\nExamples:\n>>> float(sigmoid_function(4))\n0.9820137900379085\n>>> sigmoid_function(np.array([-3, 3]))\narray([0.04742587, 0.95257413])\n>>> sigmoid_function(np.array([-3, 3, 1]))\narray([0.04742587, 0.95257413, 0.73105858])\n>>> sigmoid_function(np.array([-0.01, -2, -1.9]))\narray([0.49750002, 0.11920292, 0.13010847])\n>>> sigmoid_function(np.array([-1.3, 5.3, 12]))\narray([0.21416502, 0.9950332 , 0.99999386])\n>>> sigmoid_function(np.array([0.01, 0.02, 4.1]))\narray([0.50249998, 0.50499983, 0.9836975 ])\n>>> sigmoid_function(np.array([0.8]))\narray([0.68997448])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn cost-function "Cost function quantifies the error between predicted and expected values.\nThe cost function used in Logistic Regression is called Log Loss\nor Cross Entropy Function.\n\nJ(θ) = (1/m) * Σ [ -y * log(hθ(x)) - (1 - y) * log(1 - hθ(x)) ]\n\nWhere:\n   - J(θ) is the cost that we want to minimize during training\n   - m is the number of training examples\n   - Σ represents the summation over all training examples\n   - y is the actual binary label (0 or 1) for a given example\n   - hθ(x) is the predicted probability that x belongs to the positive class\n\n@param h: the output of sigmoid function. It is the estimated probability\nthat the input example 'x' belongs to the positive class\n\n@param y: the actual binary label associated with input example 'x'\n\nExamples:\n>>> estimations = sigmoid_function(np.array([0.3, -4.3, 8.1]))\n>>> cost_function(h=estimations,y=np.array([1, 0, 1]))\n0.18937868932131605\n>>> estimations = sigmoid_function(np.array([4, 3, 1]))\n>>> cost_function(h=estimations,y=np.array([1, 0, 0]))\n1.459999655669926\n>>> estimations = sigmoid_function(np.array([4, -3, -1]))\n>>> cost_function(h=estimations,y=np.array([1,0,0]))\n0.1266663223365915\n>>> estimations = sigmoid_function(0)\n>>> cost_function(h=estimations,y=np.array([1]))\n0.6931471805599453\n\nReferences:\n   - https://en.wikipedia.org/wiki/Logistic_regression"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn log-likelihood
  [& _args]
  ;; stub: original requires np,numpy-attr:dot,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn logistic-reg
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:shape,numpy-attr:size; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:shape,numpy-attr:size" {:type :UnsupportedOperation})))

;; NOTE: sigmoid_function stubbed (np)
;; NOTE: cost_function stubbed (np,numpy-attr:mean)
;; NOTE: log_likelihood stubbed (np,numpy-attr:dot,numpy-attr:sum)
;; NOTE: logistic_reg stubbed (np,numpy-attr:T,numpy-attr:dot,numpy-attr:shape,numpy-attr:size)
