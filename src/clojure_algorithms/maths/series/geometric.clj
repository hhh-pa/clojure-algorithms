(ns clojure-algorithms.maths.series.geometric
  "Geometric Mean
Reference :  https://en.wikipedia.org/wiki/Geometric_mean

Geometric series
Reference: https://en.wikipedia.org/wiki/Geometric_series")

(defn is-geometric-series
  "Return true when series is a geometric series.

  Throws ex-info with :type :ValueError when series is not sequential or is
  empty."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [2, 4, 8]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (if (= 1 (count series))
    true
    (try
      (let [common-ratio (/ (nth series 1) (nth series 0))]
        (every? (fn [index]
                  (= (/ (nth series (inc index)) (nth series index))
                     common-ratio))
                (range (dec (count series)))))
      (catch ArithmeticException _ false))))

(defn geometric-mean
  "Return the geometric mean of series.

  Throws ex-info with :type :ValueError when series is not sequential or is
  empty."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [2, 4, 8]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (Math/pow (double (reduce *' series)) (/ 1.0 (count series))))
