(ns clojure-algorithms.maths.series.arithmetic
  "Arithmetic mean
Reference: https://en.wikipedia.org/wiki/Arithmetic_mean

Arithmetic series
Reference: https://en.wikipedia.org/wiki/Arithmetic_series
(The URL above will redirect you to arithmetic progression)")

(defn is-arithmetic-series
  "Return true when series is an arithmetic series.

  Throws ex-info with :type :ValueError when series is not sequential or is
  empty."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [2, 4, 6]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (if (= 1 (count series))
    true
    (let [common-diff (- (nth series 1) (nth series 0))]
      (every? (fn [index]
                (= (- (nth series (inc index)) (nth series index)) common-diff))
              (range (dec (count series)))))))

(defn arithmetic-mean
  "Return the arithmetic mean of series.

  Throws ex-info with :type :ValueError when series is not sequential or is
  empty."
  [series]
  (when-not (sequential? series)
    (throw (ex-info "Input series is not valid, valid series - [2, 4, 6]"
                    {:type :ValueError})))
  (when (empty? series)
    (throw (ex-info "Input list must be a non empty list" {:type :ValueError})))
  (/ (double (reduce + series)) (count series)))
