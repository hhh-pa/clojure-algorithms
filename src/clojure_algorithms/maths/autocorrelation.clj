(ns clojure-algorithms.maths.autocorrelation
  "Autocorrelation measures the correlation of a signal with a delayed
copy of itself. It is widely used in time series analysis, signal
processing, and statistics.

Reference: https://en.wikipedia.org/wiki/Autocorrelation")

(defn autocorrelation
  "Calculate the autocorrelation of a time series at a given lag.
  data is a sequential of numbers, lag the number of time steps to shift.
  Throws :ValueError when lag is not less than the data length or when
  the data has zero variance."
  [data lag]
  (let [n (count data)]
    (when (>= lag n)
      (throw (ex-info "Lag must be less than the length of the data."
                      {:type :ValueError})))
    (let [mean (/ (reduce + 0.0 data) n)
          variance (/ (reduce + 0.0
                              (map (fn [x]
                                     (let [d (- x mean)] (* d d)))
                                   data))
                      n)]
      (when (zero? variance)
        (throw (ex-info "Variance of data is zero, autocorrelation undefined."
                        {:type :ValueError})))
      (let [covariance (/ (reduce + 0.0
                                  (map (fn [i]
                                         (* (- (nth data i) mean)
                                            (- (nth data (- i lag)) mean)))
                                       (range lag n)))
                          n)]
        (/ covariance variance)))))
