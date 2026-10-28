(ns clojure-algorithms.neural-network.optimizers.adagrad
  "Adagrad (Adaptive Gradient) optimizer.

  Adapts the learning rate per parameter from historical gradients:
    acc += g^2; p -= (lr / (sqrt(acc) + eps)) * g
  State is an immutable map threaded through `update`.
  See https://en.wikipedia.org/wiki/Stochastic_gradient_descent#AdaGrad")

(defn make-adagrad
  "Builds an Adagrad optimizer state map.
  `learning-rate` defaults to 0.01, `epsilon` to 1e-8."
  ([] (make-adagrad 0.01 1e-8))
  ([learning-rate] (make-adagrad learning-rate 1e-8))
  ([learning-rate epsilon]
   {:learning-rate (double learning-rate)
    :epsilon (double epsilon)
    :accumulated {}}))

(defn update
  "Updates `params` with `grads` for group `param-id`.
  Returns [new-state new-params]; params/grads are vectors."
  [state param-id params grads]
  (let [{:keys [learning-rate epsilon accumulated]} state
        acc (get accumulated param-id (vec (repeat (count params) 0.0)))
        acc' (mapv (fn [a g] (+ a (* g g))) acc grads)
        params' (mapv (fn [p a g]
                        (- p (* (/ learning-rate (+ (Math/sqrt a) epsilon)) g)))
                      params acc' grads)]
    [(assoc state :accumulated (assoc accumulated param-id acc')) params']))
