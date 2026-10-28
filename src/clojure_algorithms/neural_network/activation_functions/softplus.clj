(ns clojure-algorithms.neural-network.activation-functions.softplus
  "Softplus activation function.

  f(x) = ln(1 + e^x), a smooth approximation of ReLU.
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)#Softplus")

(defn softplus-value
  "Softplus of a single number."
  [x]
  (Math/log (+ 1.0 (Math/exp (double x)))))

(defn softplus
  "Applies softplus element-wise to `coll`. Returns a vector."
  [coll]
  (mapv softplus-value coll))
