(ns clojure-algorithms.neural-network.activation-functions.mish
  "Mish activation function.

  f(x) = x * tanh(softplus(x)) = x * tanh(ln(1 + e^x)).
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)#Mish")

(defn- softplus-value
  "Softplus of a single number: ln(1 + e^x)."
  [x]
  (Math/log (+ 1.0 (Math/exp (double x)))))

(defn- tanh-value
  "Hyperbolic tangent of a single number."
  [x]
  (Math/tanh (double x)))

(defn mish-value
  "Mish of a single number."
  [x]
  (* (double x) (tanh-value (softplus-value x))))

(defn mish
  "Applies the Mish activation element-wise to `coll`. Returns a vector."
  [coll]
  (mapv mish-value coll))
