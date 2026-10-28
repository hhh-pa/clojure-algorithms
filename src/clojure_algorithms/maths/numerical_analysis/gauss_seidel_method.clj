(ns clojure-algorithms.maths.numerical-analysis.gauss-seidel-method
  "Port of maths/numerical_analysis/gauss_seidel_method.py")

(declare gauss-seidel)

(defn gauss-seidel "Solve the linear system Ax = b using the Gauss-Seidel iterative method.\n\nArgs:\n    coefficients (list[list[float]]): Coefficient matrix (n x n)\n    rhs (list[float]): Right-hand side vector (n)\n    tol (float): Convergence tolerance\n    max_iter (int): Maximum number of iterations\n\nReturns:\n    list[float]: Approximate solution vector\n\nExample:\n    >>> A = [[4, 1, 2], [3, 5, 1], [1, 1, 3]]\n    >>> b = [4, 7, 3]\n    >>> gauss_seidel(A, b)\n    [0.5, 1.0, 0.5]\n\nWikipedia:\nhttps://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method"
  [& _args]
  ;; stub: original requires numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:copy" {:type :UnsupportedOperation})))

;; NOTE: gauss_seidel stubbed (numpy-attr:copy)
