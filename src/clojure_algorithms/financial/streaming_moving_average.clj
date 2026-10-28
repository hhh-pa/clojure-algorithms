(ns clojure-algorithms.financial.streaming-moving-average)

(defn make-streaming-average
  "Create a streaming moving-average calculator with window-size.
  Returns an atom holding {:window-size :window :avg}."
  [window-size]
  (when (< window-size 1)
    (throw (ex-info "Window size must be a positive integer" {})))
  (atom {:window-size window-size :window [] :avg 0.0}))

(defn sma-update!
  "Add new-value to the streaming calculator, evicting the oldest value
  once the window is full. Returns the updated average."
  [calculator new-value]
  (when (not (number? new-value))
    (throw (ex-info "Type of new_value must be numeric." {})))
  (let [{:keys [window avg]} (swap! calculator
                                    (fn [{:keys [window-size window] :as state}]
                                      (let [window (vec (take-last window-size
                                                                   (conj window (double new-value))))]
                                        (assoc state :window window
                                               :avg (/ (reduce + window) (count window))))))]
    (:avg @calculator)))

(defn sma-value
  "Current moving average value."
  [calculator]
  (:avg @calculator))
