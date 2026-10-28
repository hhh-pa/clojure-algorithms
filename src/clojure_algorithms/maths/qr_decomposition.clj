(ns clojure-algorithms.maths.qr-decomposition
  "Port of maths/qr_decomposition.py")

(declare qr-householder)

(defn qr-householder "Return a QR-decomposition of the matrix A using Householder reflection.\n\nThe QR-decomposition decomposes the matrix A of shape (m, n) into an\northogonal matrix Q of shape (m, m) and an upper triangular matrix R of\nshape (m, n).  Note that the matrix A does not have to be square.  This\nmethod of decomposing A uses the Householder reflection, which is\nnumerically stable and of complexity O(n^3).\n\nhttps://en.wikipedia.org/wiki/QR_decomposition#Using_Householder_reflections\n\nArguments:\nA -- a numpy.ndarray of shape (m, n)\n\nNote: several optimizations can be made for numeric efficiency, but this is\nintended to demonstrate how it would be represented in a mathematics\ntextbook.  In cases where efficiency is particularly important, an optimized\nversion from BLAS should be used.\n\n>>> A = np.array([[12, -51, 4], [6, 167, -68], [-4, 24, -41]], dtype=float)\n>>> Q, R = qr_householder(A)\n\n>>> # check that the decomposition is correct\n>>> np.allclose(Q@R, A)\nTrue\n\n>>> # check that Q is orthogonal\n>>> np.allclose(Q@Q.T, np.eye(A.shape[0]))\nTrue\n>>> np.allclose(Q.T@Q, np.eye(A.shape[0]))\nTrue\n\n>>> # check that R is upper triangular\n>>> np.allclose(np.triu(R), R)\nTrue"
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:copy,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:copy,numpy-attr:shape" {:type :UnsupportedOperation})))

;; NOTE: qr_householder stubbed (np,numpy-attr:T,numpy-attr:copy,numpy-attr:shape)
