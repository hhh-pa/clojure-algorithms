(ns clojure-algorithms.neural-network.optimizers.momentum-sgd
  "SGD with momentum optimizer.

    velocity = momentum * velocity - lr * gradient
    param = param + velocity
  State is an immutable map threaded through `update`.
  See https://en.wikipedia.org/wiki/Stochastic_gradient_descent#Momentum")

(defn make-momentum-sgd
  "Builds a momentum-SGD optimizer state map.
  Defaults: learning-rate 0.01, momentum 0.9."
  ([] (make-momentum-sgd 0.01 0.9))
  ([learning-rate] (make-momentum-sgd learning-rate 0.9))
  ([learning-rate momentum]
   {:learning-rate (double learning-rate)
    :momentum (double momentum)
    :velocity {}}))

(defn update
  "Updates `params` with `grads` for group `param-id`.
  Returns [new-state new-params]; params/grads are vectors."
  [state param-id params grads]
  (let [{:keys [learning-rate momentum velocity]} state
        vel (get velocity param-id (vec (repeat (count params) 0.0)))
        vel' (mapv (fn [v g] (- (* momentum v) (* learning-rate g))) vel grads)
        params' (mapv + params vel')]
    [(assoc state :velocity (assoc velocity param-id vel')) params']))
