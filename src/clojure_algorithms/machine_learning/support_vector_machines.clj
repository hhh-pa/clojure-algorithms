(ns clojure-algorithms.machine-learning.support-vector-machines
  "Port of machine_learning/support_vector_machines.py")

(declare norm-squared make-SVC SVC-linear SVC-rbf SVC-fit SVC-predict)

(defrecord SVC [regularization gamma kernel observations classes optimum offset])

(defn make-SVC "Constructor for SVC. Support Vector Classifier\n\nArgs:\n    kernel (str): kernel to use. Default: linear\n        Possible choices:\n            - linear\n    regularization: constraint for soft margin (data not linearly separable)\n        Default: unbound\n\n>>> SVC(kernel=\"asdf\")\nTraceback (most recent call last):\n    ...\nValueError: Unknown kernel: asdf\n\n>>> SVC(kernel=\"rbf\")\nTraceback (most recent call last):\n    ...\nValueError: rbf kernel requires gamma\n\n>>> SVC(kernel=\"rbf\", gamma=-1)\nTraceback (most recent call last):\n    ...\nValueError: gamma must be > 0"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn SVC-linear "Linear kernel (as if no kernel used at all)"
  [& _args]
  ;; stub: original requires ndarray,np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires ndarray,np,numpy-attr:dot" {:type :UnsupportedOperation})))

(defn SVC-rbf "RBF: Radial Basis Function Kernel\n\nNote: for more information see:\n    https://en.wikipedia.org/wiki/Radial_basis_function_kernel\n\nArgs:\n    vector1 (ndarray): first vector\n    vector2 (ndarray): second vector)\n\nReturns:\n    float: exp(-(gamma * norm_squared(vector1 - vector2)))"
  [& _args]
  ;; stub: original requires ndarray,np; pure-Clojure port not applicable
  (throw (ex-info "requires ndarray,np" {:type :UnsupportedOperation})))

(defn SVC-fit "Fits the SVC with a set of observations.\n\nArgs:\n    observations (list[ndarray]): list of observations\n    classes (ndarray): classification of each observation (in {1, -1})"
  [& _args]
  ;; stub: original requires Bounds,LinearConstraint,minimize,ndarray,np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires Bounds,LinearConstraint,minimize,ndarray,np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn SVC-predict "Get the expected class of an observation\n\nArgs:\n    observation (Vector): observation\n\nReturns:\n    int {1, -1}: expected class\n\n>>> xs = [\n...     np.asarray([0, 1]), np.asarray([0, 2]),\n...     np.asarray([1, 1]), np.asarray([1, 2])\n... ]\n>>> y = np.asarray([1, 1, -1, -1])\n>>> s = SVC()\n>>> s.fit(xs, y)\n>>> s.predict(np.asarray([0, 1]))\n1\n>>> s.predict(np.asarray([1, 1]))\n-1\n>>> s.predict(np.asarray([2, 2]))\n-1"
  [& _args]
  ;; stub: original requires ndarray; pure-Clojure port not applicable
  (throw (ex-info "requires ndarray" {:type :UnsupportedOperation})))

(defn norm-squared "Return the squared second norm of vector\nnorm_squared(v) = sum(x * x for x in v)\n\nArgs:\n    vector (ndarray): input vector\n\nReturns:\n    float: squared second norm of vector\n\n>>> int(norm_squared([1, 2]))\n5\n>>> int(norm_squared(np.asarray([1, 2])))\n5\n>>> int(norm_squared([0, 0]))\n0"
  [& _args]
  ;; stub: original requires ndarray,np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires ndarray,np,numpy-attr:dot" {:type :UnsupportedOperation})))

;; NOTE: SVC __init__ stubbed (np)
;; NOTE: SVC.__linear stubbed (ndarray,np,numpy-attr:dot)
;; NOTE: SVC.__rbf stubbed (ndarray,np)
;; NOTE: SVC.fit stubbed (Bounds,LinearConstraint,minimize,ndarray,np,numpy-attr:shape)
;; NOTE: SVC.predict stubbed (ndarray)
;; NOTE: norm_squared stubbed (ndarray,np,numpy-attr:dot)
