(ns clojure-algorithms.maths.geometric-mean
  "The Geometric Mean of n numbers is defined as the n-th root of the product
  of those numbers. It is used to measure the central tendency of the numbers.
  https://en.wikipedia.org/wiki/Geometric_mean")

(defn compute-geometric-mean
  "Returns the geometric mean of the argument numbers."
  [& args]
  (when (empty? args)
    (throw (ArithmeticException. "Divide by zero")))
  (doseq [number args]
    (when-not (number? number)
      (throw (ex-info "Not a Number" {:type :TypeError}))))
  (let [product (reduce *' 1 args)]
    ;; Cannot calculate the even root for negative product.
    (when (and (neg? product) (even? (count args)))
      (throw (ex-info "Cannot Compute Geometric Mean for these numbers."
                      {:type :ArithmeticError})))
    (let [mean (Math/pow (Math/abs (double product)) (/ 1.0 (count args)))
          ;; Python calculates complex roots for negative products
          ;; with odd roots, so negate instead.
          mean (if (neg? product) (- mean) mean)
          ;; Floating point arithmetic may give e.g. 64**(1/3) as
          ;; 3.99999996; snap back when the rounded value is exact.
          possible-mean (double (Math/round mean))]
      (if (== (Math/pow possible-mean (count args)) product)
        possible-mean
        mean))))
