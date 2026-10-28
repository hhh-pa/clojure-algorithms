(ns clojure-algorithms.neural-network.activation-functions.gaussian-error-linear-unit
  "Gaussian Error Linear Unit (GELU) activation function.

  Approximated as x * sigmoid(1.702 * x).
  See https://arxiv.org/abs/1606.08415")

(defn sigmoid-value
  "Sigmoid of a single number: 1 / (1 + e^-x)."
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))

(defn sigmoid
  "Applies sigmoid element-wise to `coll`. Returns a vector."
  [coll]
  (mapv sigmoid-value coll))

(defn gelu-value
  "GELU of a single number."
  [x]
  (* (double x) (sigmoid-value (* 1.702 (double x)))))

(defn gaussian-error-linear-unit
  "Applies GELU element-wise to `coll`. Returns a vector."
  [coll]
  (mapv gelu-value coll))
