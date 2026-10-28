(ns clojure-algorithms.neural-network.activation-functions.exponential-linear-unit
  "Exponential Linear Unit (ELU) activation function.

  f(x) = x when x > 0, else alpha * (e^x - 1).
  See https://en.wikipedia.org/wiki/Rectifier_(neural_networks)")

(defn elu-value
  "Applies ELU to a single number with hyper-parameter `alpha`."
  [x alpha]
  (if (pos? x)
    (double x)
    (* alpha (- (Math/exp (double x)) 1))))

(defn exponential-linear-unit
  "Applies the ELU activation element-wise to `coll` with `alpha`.
  Returns a vector of doubles."
  [coll alpha]
  (mapv #(elu-value % alpha) coll))
