(ns clojure-algorithms.maths.sum-of-arithmetic-series
  "Port of maths/sum_of_arithmetic_series.py.")

(defn sum-of-ap-series
  "Calculates the sum of the first 'n' terms of an arithmetic progression
  (AP) series with the first term 'a' and common difference 'd'.

  Parameters:
  a (int): The first term of the AP.
  d (int): The common difference between terms.
  n (int): The number of terms to sum.

  Returns:
  int: The sum of the first 'n' terms of the AP."
  [a d n]
  (when (<= n 0)
    (throw (ex-info "Number of terms 'n' must be a positive integer"
                    {:type :ValueError})))
  ;; Formula for the sum of an AP series: S_n = n/2 * (2a + (n-1) * d)
  (quot (* n (+ (* 2 a) (* (dec n) d))) 2))
