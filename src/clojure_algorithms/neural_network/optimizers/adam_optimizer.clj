(ns clojure-algorithms.neural-network.optimizers.adam-optimizer
  "Adam (Adaptive Moment Estimation) optimizer.

  Combines momentum with adaptive rates via bias-corrected
  first and second moment estimates.
  State is an immutable map threaded through `update`.
  See https://arxiv.org/abs/1412.6980")

(defn make-adam
  "Builds an Adam optimizer state map.
  Defaults: lr 0.001, beta1 0.9, beta2 0.999, epsilon 1e-8."
  ([] (make-adam 0.001 0.9 0.999 1e-8))
  ([learning-rate] (make-adam learning-rate 0.9 0.999 1e-8))
  ([learning-rate beta1 beta2] (make-adam learning-rate beta1 beta2 1e-8))
  ([learning-rate beta1 beta2 epsilon]
   {:learning-rate (double learning-rate)
    :beta1 (double beta1)
    :beta2 (double beta2)
    :epsilon (double epsilon)
    :m {} :v {} :t {}}))

(defn update
  "Updates `params` with `grads` for group `param-id`.
  Returns [new-state new-params]; params/grads are vectors."
  [state param-id params grads]
  (let [{:keys [learning-rate beta1 beta2 epsilon m v t]} state
        n (count params)
        zeros (vec (repeat n 0.0))
        m0 (get m param-id zeros)
        v0 (get v param-id zeros)
        t1 (inc (get t param-id 0))
        m1 (mapv (fn [mi g] (+ (* beta1 mi) (* (- 1 beta1) g))) m0 grads)
        v1 (mapv (fn [vi g] (+ (* beta2 vi) (* (- 1 beta2) g g))) v0 grads)
        bc1 (- 1 (Math/pow beta1 t1))
        bc2 (- 1 (Math/pow beta2 t1))
        params' (mapv (fn [p mi vi]
                        (let [mhat (/ mi bc1)
                              vhat (/ vi bc2)]
                          (- p (* learning-rate (/ mhat (+ (Math/sqrt vhat) epsilon))))))
                      params m1 v1)]
    [(assoc state
            :m (assoc m param-id m1)
            :v (assoc v param-id v1)
            :t (assoc t param-id t1))
     params']))
