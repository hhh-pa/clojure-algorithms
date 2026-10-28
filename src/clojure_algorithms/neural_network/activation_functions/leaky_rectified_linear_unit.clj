(ns clojure-algorithms.neural-network.activation-functions.leaky-rectified-linear-unit
  "Leaky Rectified Linear Unit (Leaky ReLU) activation function.

  f(x) = x when x > 0, else alpha * x.
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)#Leaky_ReLU")

(defn leaky-relu-value
  "Applies Leaky ReLU to a single number with slope `alpha`."
  [x alpha]
  (if (pos? x) (double x) (* alpha (double x))))

(defn leaky-rectified-linear-unit
  "Applies Leaky ReLU element-wise to `coll` with slope `alpha`.
  Returns a vector."
  [coll alpha]
  (mapv #(leaky-relu-value % alpha) coll))
