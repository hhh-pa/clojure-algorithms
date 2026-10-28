(ns clojure-algorithms.machine-learning.ordinary-least-squares-regression
  "Ordinary Least Squares Regression (OLSR):\n\nOrdinary Least Squares Regression (OLSR) is a statistical method for\nestimating the parameters of a linear regression model.\nIt is the most commonly used regression method,\nand it is based on the principle of minimizing\nthe sum of the squared residuals.\n\nBelow is a simple implementation of OLSR\nwithout using any external libraries.\n\nWIKI: https://en.wikipedia.org/wiki/Ordinary_least_squares")

(declare ols-regression)

(defn ols-regression "Performs Ordinary Least Squares Regression (OLSR) on the given data.\n\nArgs:\n    x: The independent variable.\n    y: The dependent variable.\n\nReturns:\n    a (float): The intercept of the regression line.\n    b (float): The slope of the regression line.\n\nExamples:\n>>> x = np.array([1, 2, 3, 4, 5])\n>>> y = np.array([2, 4, 6, 8, 10])\n>>> a, b = ols_regression(x, y)\n>>> float(a)  # Intercept should be 0.0\n0.0\n>>> float(round(b, 2))  # Slope should be 2.0\n2.0"
  [& _args]
  ;; stub: original requires np,numpy-attr:mean,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:mean,numpy-attr:sum" {:type :UnsupportedOperation})))

;; NOTE: ols_regression stubbed (np,numpy-attr:mean,numpy-attr:sum)
