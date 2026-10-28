(ns clojure-algorithms.neural-network.activation-functions.scaled-exponential-linear-unit
  "Scaled Exponential Linear Unit (SELU) activation function.

  f(x) = lambda * x when x > 0, else lambda * alpha * (e^x - 1).
  Defaults: alpha = 1.6732, lambda = 1.0507.
  See https://iq.opengenus.org/scaled-exponential-linear-unit/")

(def default-alpha
  "Default SELU alpha hyper-parameter."
  1.6732)

(def default-lambda
  "Default SELU lambda hyper-parameter."
  1.0507)

(defn selu-value
  "SELU of a single number with `alpha` and `lambda_`."
  [x alpha lambda_]
  (if (pos? x)
    (* lambda_ (double x))
    (* lambda_ alpha (- (Math/exp (double x)) 1))))

(defn scaled-exponential-linear-unit
  "Applies SELU element-wise to `coll`.
  With one arg uses default alpha/lambda; optionally pass both."
  ([coll]
   (scaled-exponential-linear-unit coll default-alpha default-lambda))
  ([coll alpha lambda_]
   (mapv #(selu-value % alpha lambda_) coll)))
