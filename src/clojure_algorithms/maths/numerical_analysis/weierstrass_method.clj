(ns clojure-algorithms.maths.numerical-analysis.weierstrass-method
  "Port of maths/numerical_analysis/weierstrass_method.py")

(declare weierstrass-method)

(defn weierstrass-method "Approximates all complex roots of a polynomial using the\nWeierstrass (Durand-Kerner) method.\nArgs:\n    polynomial: A function that takes a NumPy array of complex numbers and returns\n                the polynomial values at those points.\n    degree: Degree of the polynomial (number of roots to find). Must be ≥ 1.\n    roots:  Optional initial guess as a NumPy array of complex numbers.\n            Must have length equal to 'degree'.\n            If None, perturbed complex roots of unity are used.\n    max_iter: Number of iterations to perform (default: 100).\n\nReturns:\n    np.ndarray: Array of approximated complex roots.\n\nRaises:\n    ValueError: If degree < 1, or if initial roots length doesn't match the degree.\n\nNote:\n    - Root updates are clipped to prevent numerical overflow.\n\nExample:\n    >>> import numpy as np\n    >>> def check(poly, degree, expected):\n    ...     roots = weierstrass_method(poly, degree)\n    ...     return np.allclose(np.sort(roots), np.sort(expected))\n\n    >>> check(\n    ...     lambda x: x**2 - 1,\n    ...     2,\n    ...     np.array([-1, 1]))\n    True\n\n    >>> check(\n    ...     lambda x: x**3 - 4.5*x**2 + 5.75*x - 1.875,\n    ...     3,\n    ...     np.array([1.5, 0.5, 2.5])\n    ... )\n    True\n\nSee Also:\n    https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method"
  [& _args]
  ;; stub: original requires complex-literal,np,numpy-attr:astype,numpy-attr:clip,numpy-attr:prod,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires complex-literal,np,numpy-attr:astype,numpy-attr:clip,numpy-attr:prod,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: weierstrass_method stubbed (complex-literal,np,numpy-attr:astype,numpy-attr:clip,numpy-attr:prod,numpy-attr:shape)
