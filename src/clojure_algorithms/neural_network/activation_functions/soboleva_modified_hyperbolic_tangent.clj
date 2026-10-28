(ns clojure-algorithms.neural-network.activation-functions.soboleva-modified-hyperbolic-tangent
  "Soboleva Modified Hyperbolic Tangent (SMHT) activation function.

  f(x) = (e^(a*x) - e^(-b*x)) / (e^(c*x) + e^(-d*x)).
  See https://en.wikipedia.org/wiki/Soboleva_modified_hyperbolic_tangent")

(defn smht-value
  "SMHT of a single number with parameters `a`, `b`, `c`, `d`."
  [x a b c d]
  (let [xd (double x)
        num (- (Math/exp (* a xd)) (Math/exp (* (- b) xd)))
        den (+ (Math/exp (* c xd)) (Math/exp (* (- d) xd)))]
    (/ num den)))

(defn soboleva-modified-hyperbolic-tangent
  "Applies SMHT element-wise to `coll` with parameters `a`, `b`, `c`, `d`.
  Returns a vector."
  [coll a b c d]
  (mapv #(smht-value % a b c d) coll))
