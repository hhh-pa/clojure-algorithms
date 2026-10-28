(ns clojure-algorithms.neural-network.activation-functions.binary-step
  "Binary Step activation function.

  A neuron is activated (1) when the input is zero or positive,
  otherwise it is deactivated (0).
  See https://en.wikipedia.org/wiki/Activation_function")

(defn binary-step-value
  "Returns 1 when x is zero or positive, else 0."
  [x]
  (if (>= x 0) 1 0))

(defn binary-step
  "Applies the binary step function element-wise to `coll`.
  Returns a vector of 0s and 1s."
  [coll]
  (mapv binary-step-value coll))
