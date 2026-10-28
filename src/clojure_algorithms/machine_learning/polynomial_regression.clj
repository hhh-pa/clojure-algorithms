(ns clojure-algorithms.machine-learning.polynomial-regression
  "Polynomial regression via ordinary least squares.

  The Vandermonde design matrix and prediction from fitted parameters are
  plain arithmetic and fully ported. Fitting itself needs the SVD-based
  Moore-Penrose pseudoinverse (and matrix rank), which has no stdlib-only
  equivalent, so fit and the seaborn/matplotlib demo are honestly stubbed.

  References:
  https://en.wikipedia.org/wiki/Polynomial_regression
  https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn make-PolynomialRegression
  "Create an unfitted model of the given non-negative degree.
  Throws :ValueError for a negative degree."
  [degree]
  (when (< degree 0)
    (throw (ex-info "Polynomial degree must be non-negative"
                    {:type :ValueError})))
  {:degree degree :params nil})

(defn- int-pow
  [x j]
  (reduce * 1 (repeat j x)))

(defn PolynomialRegression-design-matrix
  "Vandermonde matrix: row [1 x x^2 ... x^degree] per input value.
  Throws :ValueError unless data is one-dimensional."
  [data degree]
  (when (some sequential? data)
    (throw (ex-info "Data must have dimensions N x 1" {:type :ValueError})))
  (mapv (fn [x] (mapv #(int-pow x %) (range (inc degree)))) data))

(defn PolynomialRegression-fit
  "STUB: needs SVD (Moore-Penrose pseudoinverse) plus matrix rank."
  [& _args]
  (unsupported! "np.linalg.pinv"))

(defn PolynomialRegression-predict
  "Evaluate y = Xβ for the fitted params.
  Throws :ArithmeticError when the model has not been fitted yet."
  [self data]
  (when (nil? (:params self))
    (throw (ex-info "Predictor hasn't been fit yet" {:type :ArithmeticError})))
  (mapv #(reduce + (map * % (:params self)))
        (PolynomialRegression-design-matrix data (:degree self))))

(defn main
  "STUB: seaborn demo (needs network data plus matplotlib)."
  [& _args]
  (unsupported! "sns"))

(def PolynomialRegression---slots-- ["degree" "params"])
