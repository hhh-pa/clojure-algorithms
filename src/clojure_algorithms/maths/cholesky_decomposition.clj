(ns clojure-algorithms.maths.cholesky-decomposition
  "Port of maths/cholesky_decomposition.py")

(declare cholesky-decomposition solve-cholesky)

(defn cholesky-decomposition "Return a Cholesky decomposition of the matrix A.\n\nThe Cholesky decomposition decomposes the square, positive definite matrix A\ninto a lower triangular matrix L such that A = L L^T.\n\nhttps://en.wikipedia.org/wiki/Cholesky_decomposition\n\nArguments:\nA -- a numpy.ndarray of shape (n, n)\n\n>>> A = np.array([[4, 12, -16], [12, 37, -43], [-16, -43, 98]], dtype=float)\n>>> L = cholesky_decomposition(A)\n>>> np.allclose(L, np.array([[2, 0, 0], [6, 1, 0], [-8, 5, 3]]))\nTrue\n\n>>> # check that the decomposition is correct\n>>> np.allclose(L @ L.T, A)\nTrue\n\n>>> # check that L is lower triangular\n>>> np.allclose(np.tril(L), L)\nTrue\n\nThe Cholesky decomposition can be used to solve the linear system A x = y.\n\n>>> x_true = np.array([1, 2, 3], dtype=float)\n>>> y = A @ x_true\n>>> x = solve_cholesky(L, y)\n>>> np.allclose(x, x_true)\nTrue\n\nIt can also be used to solve multiple equations A X = Y simultaneously.\n\n>>> X_true = np.random.rand(3, 3)\n>>> Y = A @ X_true\n>>> X = solve_cholesky(L, Y)\n>>> np.allclose(X, X_true)\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn solve-cholesky "Given a Cholesky decomposition L L^T = A of a matrix A, solve the\nsystem of equations A X = Y where the right-hand side Y is either\na matrix or a vector.\n\n>>> L = np.array([[2, 0], [3, 4]], dtype=float)\n>>> Y = np.array([[22, 54], [81, 193]], dtype=float)\n>>> X = solve_cholesky(L, Y)\n>>> np.allclose(X, np.array([[1, 3], [3, 7]], dtype=float))\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:copy,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:copy,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: cholesky_decomposition stubbed (np,numpy-attr:T,numpy-attr:shape,numpy-attr:sum)
;; NOTE: solve_cholesky stubbed (np,numpy-attr:copy,numpy-attr:ravel,numpy-attr:reshape,numpy-attr:shape)
