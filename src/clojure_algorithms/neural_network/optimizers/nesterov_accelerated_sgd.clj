(ns clojure-algorithms.neural-network.optimizers.nesterov-accelerated-sgd
  "Nesterov Accelerated Gradient (NAG) optimizer.

  Looks ahead and computes gradients at the anticipated position:
    v' = mu * v - lr * g(lookahead); p += -mu * v + (1 + mu) * v'
  State is an immutable map threaded through `update`.
  See https://cs231n.github.io/neural-networks-3/#sgd")

(defn make-nesterov
  "Builds a Nesterov optimizer state map.
  Defaults: learning-rate 0.01, momentum 0.9."
  ([] (make-nesterov 0.01 0.9))
  ([learning-rate] (make-nesterov learning-rate 0.9))
  ([learning-rate momentum]
   {:learning-rate (double learning-rate)
    :momentum (double momentum)
    :velocity {}}))

(defn update
  "Updates `params` with lookahead `grads` for group `param-id`.
  Returns [new-state new-params]; params/grads are vectors."
  [state param-id params grads]
  (let [{:keys [learning-rate momentum velocity]} state
        vel (get velocity param-id (vec (repeat (count params) 0.0)))
        vel' (mapv (fn [v g] (- (* momentum v) (* learning-rate g))) vel grads)
        params' (mapv (fn [p vp vn] (+ (- p (* momentum vp)) (* (+ 1 momentum) vn)))
                      params vel vel')]
    [(assoc state :velocity (assoc velocity param-id vel')) params']))
