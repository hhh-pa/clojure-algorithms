(ns clojure-algorithms.maths.series.harmonic
  "Harmonic mean
Reference: https://en.wikipedia.org/wiki/Harmonic_mean

Harmonic series
Reference: https://en.wikipedia.org/wiki/Harmonic_series(mathematics)")

(defn is-harmonic-series
  "Return true when series is a harmonic series.

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
    (let [rec-series (mapv (fn [v]
                             (when (= 0 v)
                               (throw (ex-info "Input series cannot have 0 as an element"
                                               {:type :ValueError})))
                             (/ 1.0 (double v)))
                           series)
          common-diff (- (nth rec-series 1) (nth rec-series 0))]
      (every? (fn [index]
                (= (- (nth rec-series index)
                      (nth rec-series (dec index)))
                   common-diff))
              (range 2 (count series))))))

(defn harmonic-mean
  "Return the harmonic mean of series.

  Throws ex-info with :type :ValueError when series is not sequential or is
  empty."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [2, 4, 6]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (/ (double (count series)) (reduce + (map #(/ 1.0 (double %)) series))))
