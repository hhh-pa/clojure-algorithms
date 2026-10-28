(ns clojure-algorithms.neural-network.activation-functions.softsign
  "Softsign activation function.

  f(x) = x / (1 + |x|), mapping inputs into (-1, 1).
  See https://en.wikipedia.org/wiki/Activation_function")

(defn softsign-value
  "Softsign of a single number."
  [x]
  (/ (double x) (+ 1.0 (Math/abs (double x)))))

(defn softsign
  "Applies softsign element-wise to `coll`. Returns a vector."
  [coll]
  (mapv softsign-value coll))
