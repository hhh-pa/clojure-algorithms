(ns clojure-algorithms.machine-learning.rmsprop
  "RMSprop (Root Mean Square Propagation) optimizer.

  Maintains a moving average of squared gradients to normalize each update:
    v(t) = rho * v(t-1) + (1 - rho) * gradient^2
    param = param - (learning_rate / sqrt(v(t) + epsilon)) * gradient

  Reference: https://en.wikipedia.org/wiki/Stochastic_gradient_descent#RMSProp")

(defn rmsprop
  "Perform one step of RMSprop, returning the updated parameters.

  moving-avg carries the running average of squared gradients across calls:
  nil (default) starts from zeros, a sequential is used as the initial value,
  and an atom is updated in place (matching the upstream Python behaviour).

  Throws ex-info with :type :ValueError when params/gradients differ in
  length, or when learning-rate, rho or epsilon are out of range."
  ([params gradients learning-rate]
   (rmsprop params gradients learning-rate 0.9 1e-8 nil))
  ([params gradients learning-rate rho]
   (rmsprop params gradients learning-rate rho 1e-8 nil))
  ([params gradients learning-rate rho epsilon]
   (rmsprop params gradients learning-rate rho epsilon nil))
  ([params gradients learning-rate rho epsilon moving-avg]
   (when (not= (count params) (count gradients))
     (throw (ex-info (str "params and gradients must have the same length, got "
                          (count params) " and " (count gradients))
                     {:type :ValueError})))
   (when (<= learning-rate 0)
     (throw (ex-info (str "learning_rate must be positive, got " learning-rate)
                     {:type :ValueError})))
   (when (not (and (<= 0.0 rho) (< rho 1.0)))
     (throw (ex-info (str "rho must be in [0, 1), got " rho) {:type :ValueError})))
   (when (<= epsilon 0)
     (throw (ex-info (str "epsilon must be positive, got " epsilon)
                     {:type :ValueError})))
   (let [avg (cond (nil? moving-avg) (vec (repeat (count params) 0.0))
                   (instance? clojure.lang.Atom moving-avg) @moving-avg
                   :else (vec moving-avg))
         [updated new-avg]
         (reduce (fn [[ps avgs] [i param grad]]
                   (let [v (+ (* rho (nth avgs i)) (* (- 1 rho) grad grad))
                         p (- param (* (/ learning-rate (Math/sqrt (+ v epsilon)))
                                       grad))]
                     [(conj ps p) (assoc avgs i v)]))
                 [[] avg]
                 (map vector (range) params gradients))]
     (when (instance? clojure.lang.Atom moving-avg)
       (reset! moving-avg new-avg))
     updated)))
