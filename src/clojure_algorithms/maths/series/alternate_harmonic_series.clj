(ns clojure-algorithms.maths.series.alternate-harmonic-series
  "Check whether a series is an alternate harmonic series.
https://en.wikipedia.org/wiki/Harmonic_series_(mathematics)")

(defn is-alternate-harmonic-series
  "Return true when series is an alternate harmonic series.

  Throws ex-info with :type :ValueError when series is not sequential, is
  empty, or contains 0."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [1, 2/3, 2]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (if (and (= 1 (count series)) (not= 0 (first series)))
    true
    (let [reciprocal (mapv (fn [v]
                             (when (= 0 v)
                               (throw (ex-info "Input series cannot have 0 as an element"
                                               {:type :ValueError})))
                             (/ 1.0 (double v)))
                           series)
          common-diff (- (Math/abs (nth reciprocal 1))
                         (Math/abs (nth reciprocal 0)))]
      (every? (fn [index]
                (and (= (- (Math/abs (nth reciprocal index))
                           (Math/abs (nth reciprocal (dec index))))
                        common-diff)
                     (neg? (* (nth reciprocal index)
                              (nth reciprocal (dec index))))))
              (range 2 (count series))))))
