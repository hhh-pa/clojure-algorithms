(ns clojure-algorithms.machine-learning.polynomial-regression
  "Polynomial regression is a type of regression analysis that models the relationship\nbetween a predictor x and the response y as an mth-degree polynomial:\n\ny = β₀ + β₁x + β₂x² + ... + βₘxᵐ + ε\n\nBy treating x, x², ..., xᵐ as distinct variables, we see that polynomial regression is a\nspecial case of multiple linear regression. Therefore, we can use ordinary least squares\n(OLS) estimation to estimate the vector of model parameters β = (β₀, β₁, β₂, ..., βₘ)\nfor polynomial regression:\n\nβ = (XᵀX)⁻¹Xᵀy = X⁺y\n\nwhere X is the design matrix, y is the response vector, and X⁺ denotes the Moore-Penrose\npseudoinverse of X. In the case of polynomial regression, the design matrix is\n\n    |1  x₁  x₁² ⋯ x₁ᵐ|\nX = |1  x₂  x₂² ⋯ x₂ᵐ|\n    |⋮  ⋮   ⋮   ⋱ ⋮  |\n    |1  xₙ  xₙ² ⋯  xₙᵐ|\n\nIn OLS estimation, inverting XᵀX to compute X⁺ can be very numerically unstable. This\nimplementation sidesteps this need to invert XᵀX by computing X⁺ using singular value\ndecomposition (SVD):\n\nβ = VΣ⁺Uᵀy\n\nwhere UΣVᵀ is an SVD of X.\n\nReferences:\n    - https://en.wikipedia.org/wiki/Polynomial_regression\n    - https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse\n    - https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares\n    - https://en.wikipedia.org/wiki/Singular_value_decomposition"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare main make-PolynomialRegression PolynomialRegression-design-matrix PolynomialRegression-fit PolynomialRegression-predict PolynomialRegression---slots--)

(defrecord PolynomialRegression [degree params])

(defn make-PolynomialRegression "Constructor for PolynomialRegression."
  [degree] (let [self (atom {})] (try (when (py-truthy (py-lt degree 0)) (throw (ex-info (py-str "Polynomial degree must be non-negative") {:type :ValueError}))) (swap! self (fn [__c] (py-assoc-in __c [:degree] degree))) (swap! self (fn [__c] (py-assoc-in __c [:params] nil))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->PolynomialRegression @self)))

(defn PolynomialRegression-design-matrix "Constructs a polynomial regression design matrix for the given input data. For\ninput data x = (x₁, x₂, ..., xₙ) and polynomial degree m, the design matrix is\nthe Vandermonde matrix\n\n    |1  x₁  x₁² ⋯ x₁ᵐ|\nX = |1  x₂  x₂² ⋯ x₂ᵐ|\n    |⋮  ⋮   ⋮   ⋱ ⋮  |\n    |1  xₙ  xₙ² ⋯  xₙᵐ|\n\nReference: https://en.wikipedia.org/wiki/Vandermonde_matrix\n\n@param data:    the input predictor values x, either for model fitting or for\n                prediction\n@param degree:  the polynomial degree m\n@returns:       the Vandermonde matrix X (see above)\n@raises ValueError: if input data is not N x 1\n\n>>> x = np.array([0, 1, 2])\n>>> PolynomialRegression._design_matrix(x, degree=0)\narray([[1],\n       [1],\n       [1]])\n>>> PolynomialRegression._design_matrix(x, degree=1)\narray([[1, 0],\n       [1, 1],\n       [1, 2]])\n>>> PolynomialRegression._design_matrix(x, degree=2)\narray([[1, 0, 0],\n       [1, 1, 1],\n       [1, 2, 4]])\n>>> PolynomialRegression._design_matrix(x, degree=3)\narray([[1, 0, 0, 0],\n       [1, 1, 1, 1],\n       [1, 2, 4, 8]])\n>>> PolynomialRegression._design_matrix(np.array([[0, 0], [0 , 0]]), degree=3)\nTraceback (most recent call last):\n...\nValueError: Data must have dimensions N x 1"
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn PolynomialRegression-fit "Computes the polynomial regression model parameters using ordinary least squares\n(OLS) estimation:\n\nβ = (XᵀX)⁻¹Xᵀy = X⁺y\n\nwhere X⁺ denotes the Moore-Penrose pseudoinverse of the design matrix X. This\nfunction computes X⁺ using singular value decomposition (SVD).\n\nReferences:\n    - https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse\n    - https://en.wikipedia.org/wiki/Singular_value_decomposition\n    - https://en.wikipedia.org/wiki/Multicollinearity\n\n@param x_train: the predictor values x for model fitting\n@param y_train: the response values y for model fitting\n@raises ArithmeticError:    if X isn't full rank, then XᵀX is singular and β\n                            doesn't exist\n\n>>> x = np.array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])\n>>> y = x**3 - 2 * x**2 + 3 * x - 5\n>>> poly_reg = PolynomialRegression(degree=3)\n>>> poly_reg.fit(x, y)\n>>> poly_reg.params\narray([-5.,  3., -2.,  1.])\n>>> poly_reg = PolynomialRegression(degree=20)\n>>> poly_reg.fit(x, y)\nTraceback (most recent call last):\n...\nArithmeticError: Design matrix is not full rank, can't compute coefficients\n\nMake sure errors don't grow too large:\n>>> coefs = np.array([-250, 50, -2, 36, 20, -12, 10, 2, -1, -15, 1])\n>>> y = PolynomialRegression._design_matrix(x, len(coefs) - 1) @ coefs\n>>> poly_reg = PolynomialRegression(degree=len(coefs) - 1)\n>>> poly_reg.fit(x, y)\n>>> np.allclose(poly_reg.params, coefs, atol=10e-3)\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn PolynomialRegression-predict "Computes the predicted response values y for the given input data by\nconstructing the design matrix X and evaluating y = Xβ.\n\n@param data:    the predictor values x for prediction\n@returns:       the predicted response values y = Xβ\n@raises ArithmeticError:    if this function is called before the model\n                            parameters are fit\n\n>>> x = np.array([0, 1, 2, 3, 4])\n>>> y = x**3 - 2 * x**2 + 3 * x - 5\n>>> poly_reg = PolynomialRegression(degree=3)\n>>> poly_reg.fit(x, y)\n>>> poly_reg.predict(np.array([-1]))\narray([-11.])\n>>> poly_reg.predict(np.array([-2]))\narray([-27.])\n>>> poly_reg.predict(np.array([6]))\narray([157.])\n>>> PolynomialRegression(degree=3).predict(x)\nTraceback (most recent call last):\n...\nArithmeticError: Predictor hasn't been fit yet"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn main "Fit a polynomial regression model to predict fuel efficiency using seaborn's mpg\ndataset\n\n>>> pass    # Placeholder, function is only for demo purposes"
  [& _args]
  ;; stub: original requires np,plt,sns; pure-Clojure port not applicable
  (throw (ex-info "requires np,plt,sns" {:type :UnsupportedOperation})))

(def PolynomialRegression---slots-- ["degree" "params"])

;; NOTE: PolynomialRegression._design_matrix stubbed (np,numpy-attr:shape)
;; NOTE: PolynomialRegression.fit stubbed (np,numpy-attr:shape)
;; NOTE: PolynomialRegression.predict stubbed (np)
;; NOTE: main stubbed (np,plt,sns)
