(ns clojure-algorithms.neural-network.activation-functions.squareplus
  "Squareplus activation function.

  f(x) = (x + sqrt(x^2 + b)) / 2, where b is the curved-region size.
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)#Squareplus")

(defn squareplus-value
  "Squareplus of a single number with curved-region size `beta`."
  [x beta]
  (/ (+ (double x) (Math/sqrt (+ (* (double x) (double x)) beta))) 2.0))

(defn squareplus
  "Applies Squareplus element-wise to `coll` with `beta`. Returns a vector."
  [coll beta]
  (mapv #(squareplus-value % beta) coll))
