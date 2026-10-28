(ns clojure-algorithms.machine-learning.polynomial-regression
  "Polynomial regression is a type of regression analysis that models the relationship\nbetween a predictor x and the response y as an mth-degree polynomial:\n\ny = β₀ + β₁x + β₂x² + ... + βₘxᵐ + ε\n\nBy treating x, x², ..., xᵐ as distinct variables, we see that polynomial regression is a\nspecial case of multiple linear regression. Therefore, we can use ordinary least squares\n(OLS) estimation to estimate the vector of model parameters β = (β₀, β₁, β₂, ..., βₘ)\nfor polynomial regression:\n\nβ = (XᵀX)⁻¹Xᵀy = X⁺y\n\nwhere X is the design matrix, y is the response vector, and X⁺ denotes the Moore-Penrose\npseudoinverse of X. In the case of polynomial regression, the design matrix is\n\n    |1  x₁  x₁² ⋯ x₁ᵐ|\nX = |1  x₂  x₂² ⋯ x₂ᵐ|\n    |⋮  ⋮   ⋮   ⋱ ⋮  |\n    |1  xₙ  xₙ² ⋯  xₙᵐ|\n\nIn OLS estimation, inverting XᵀX to compute X⁺ can be very numerically unstable. This\nimplementation sidesteps this need to invert XᵀX by computing X⁺ using singular value\ndecomposition (SVD):\n\nβ = VΣ⁺Uᵀy\n\nwhere UΣVᵀ is an SVD of X.\n\nReferences:\n    - https://en.wikipedia.org/wiki/Polynomial_regression\n    - https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse\n    - https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares\n    - https://en.wikipedia.org/wiki/Singular_value_decomposition"
  (:require [clojure.string :as str]))

(declare main make-PolynomialRegression PolynomialRegression-design-matrix PolynomialRegression-fit PolynomialRegression-predict PolynomialRegression---slots--)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

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
