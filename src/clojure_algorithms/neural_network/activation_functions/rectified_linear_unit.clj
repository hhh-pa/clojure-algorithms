(ns clojure-algorithms.neural-network.activation-functions.rectified-linear-unit
  "Rectified Linear Unit (ReLU) activation function.

  f(x) = max(0, x).
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)")

(defn relu-value
  "ReLU of a single number."
  [x]
  (max 0.0 (double x)))

(defn relu
  "Applies ReLU element-wise to `coll`. Returns a vector."
  [coll]
  (mapv relu-value coll))
