(ns clojure-algorithms.neural-network.activation-functions.swish
  "Swish / Sigmoid Linear Unit (SiLU) activation function.

  swish(x, b) = x * sigmoid(b * x); SiLU is swish with b = 1.
  See https://arxiv.org/abs/1710.05941")

(defn sigmoid-value
  "Sigmoid of a single number: 1 / (1 + e^-x)."
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))

(defn sigmoid
  "Applies sigmoid element-wise to `coll`. Returns a vector."
  [coll]
  (mapv sigmoid-value coll))

(defn silu-value
  "Sigmoid Linear Unit of a single number."
  [x]
  (* (double x) (sigmoid-value x)))

(defn sigmoid-linear-unit
  "Applies SiLU (swish with beta = 1) element-wise to `coll`."
  [coll]
  (mapv silu-value coll))

(defn swish-value
  "Swish of a single number with trainable parameter `beta`."
  [x beta]
  (* (double x) (sigmoid-value (* beta (double x)))))

(defn swish
  "Applies swish element-wise to `coll` with `trainable-parameter`."
  [coll trainable-parameter]
  (mapv #(swish-value % trainable-parameter) coll))
