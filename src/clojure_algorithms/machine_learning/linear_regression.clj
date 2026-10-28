(ns clojure-algorithms.machine-learning.linear-regression
  "Linear regression by steepest gradient descent.

  Only the numeric core is ported: dataset collection needs network access
  and the plots need matplotlib, so those entry points are honestly stubbed.")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn- dot
  [a b]
  (reduce + (map * a b)))

(defn- transpose
  [matrix]
  (apply mapv vector matrix))

(defn collect-dataset
  "STUB: downloads the CSGO dataset over the network (needs httpx2)."
  [& _args]
  (unsupported! "httpx2"))

(defn run-steep-gradient-descent
  "One gradient step: theta - (alpha/n) * (theta.X' - y).X.

  >>> (run-steep-gradient-descent [[1 2] [3 4]] [5 6] 2 0.01 [0.1 0.2])
  [0.196 0.343]"
  [data-x data-y len-data alpha theta]
  (let [prod (mapv #(- (dot theta %) %2) data-x data-y)
        sum-grad (mapv #(dot prod %) (transpose data-x))]
    (mapv #(- %1 (* (/ alpha len-data) %2)) theta sum-grad)))

(defn sum-of-square-error
  "Sum of squared residuals for theta on the data."
  [data-x data-y theta]
  (let [prod (mapv #(- (dot theta %) %2) data-x data-y)]
    (reduce + (map #(* % %) prod))))

(defn run-linear-regression
  "Fit by 100000 gradient steps, printing the error each iteration.
  Returns [theta err] (err stays empty, as upstream)."
  [data-x data-y]
  (let [iterations 100000
        alpha 0.0001550
        no-features (count (first data-x))
        len-data (dec (count data-x))]
    (loop [i 0
           theta (vec (repeat no-features 0.0))]
      (if (>= i iterations)
        [theta []]
        (let [theta (run-steep-gradient-descent data-x data-y len-data alpha
                                                theta)
              error (sum-of-square-error data-x data-y theta)]
          (println (format "At Iteration %d - Error is %.5f" (inc i) error))
          (when (zero? (mod i 1000))
            (println (format "At Iteration %d - Error is %.5f" (inc i) error)))
          (recur (inc i) theta))))))

(defn mean-absolute-error
  "Mean absolute error between predicted and original outputs.

  >>> (mean-absolute-error [3 -0.5 2 7] [2.5 0.0 2 8])
  0.5"
  [predicted-y original-y]
  (/ (double (reduce + (map #(Math/abs (- (double %1) (double %2)))
                            original-y predicted-y)))
     (count original-y)))

(defn plot-regression
  "STUB: needs matplotlib (regression-line scatter plot)."
  [& _args]
  (unsupported! "plt"))

(defn plot-loss
  "STUB: needs matplotlib (training-loss curve)."
  [& _args]
  (unsupported! "plt"))

(defn main
  "STUB: needs the network dataset plus matplotlib driver."
  [& _args]
  (unsupported! "httpx2"))
