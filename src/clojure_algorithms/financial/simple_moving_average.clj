(ns clojure-algorithms.financial.simple-moving-average)

(defn simple-moving-average
  "Simple moving average over data with window-size.
  Returns a vector the same length as data; the first (window-size - 1)
  entries are nil (not enough data)."
  [data window-size]
  (when (< window-size 1)
    (throw (ex-info "Window size must be a positive integer" {})))
  (vec (map-indexed (fn [i _]
                      (when (>= i (dec window-size))
                        (let [window (subvec (vec data) (- i (dec window-size)) (inc i))]
                          (/ (double (reduce + window)) window-size))))
                    data)))
