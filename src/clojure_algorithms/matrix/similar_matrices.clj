(ns clojure-algorithms.matrix.similar-matrices
  "Determine whether two square matrices are similar.\n\nTwo square matrices :math:`A` and :math:`B` of the same size are similar if\nthere exists an invertible matrix :math:`P` such that :math:`P^{-1} A P = B`.\nThis implementation relies on SymPy to compute the Jordan canonical form of\nboth matrices.  Two matrices are similar precisely when their Jordan forms are\nequal up to permutation of the Jordan blocks.\n* https://en.wikipedia.org/wiki/Jordan_matrix\n* https://en.wikipedia.org/wiki/Jordan_normal_form\n\nExamples\n--------\n>>> are_similar_matrices([[3, 1], [0, 3]], [[3, 0], [0, 3]])\nFalse\n>>> from sympy import Matrix\n>>> matrix_a = [[3, 1], [0, 3]]\n>>> transform = Matrix([[1, 1], [0, 1]])\n>>> matrix_b = (transform.inv() * Matrix(matrix_a) * transform).tolist()\n>>> are_similar_matrices(matrix_a, matrix_b)\nTrue\n>>> are_similar_matrices(\n...     [[1, 2, 0], [0, 1, 0], [0, 0, 3]],\n...     [[1, 0, 0], [0, 1, 0], [0, 0, 3]],\n... )\nFalse\n>>> are_similar_matrices([[1, 2], [0, 1]], [[1, 2, 0], [0, 1, 0]])\nTraceback (most recent call last):\n    ...\nValueError: both matrices must be square with the same dimensions")

(declare -as-square-matrix -jordan-signature are-similar-matrices --all-- MatrixLike)

(defn -as-square-matrix "Return a SymPy matrix after validating that ``matrix`` is square.\n\nParameters\n----------\nmatrix:\n    Nested sequences (or a SymPy matrix) describing the matrix entries.\nsimplify_entries:\n    When ``True`` each entry is passed through :func:`sympy.nsimplify`\n    which helps treat values such as ``0.5`` and ``1/2`` as the same.\n\nRaises\n------\nTypeError\n    If ``matrix`` cannot be converted into a SymPy matrix.\nValueError\n    If ``matrix`` is not square."
  [& _args]
  ;; stub: original requires Matrix,nsimplify; pure-Clojure port not applicable
  (throw (ex-info "requires Matrix,nsimplify" {:type :UnsupportedOperation})))

(defn -jordan-signature "Return a hashable representation of the Jordan form of ``matrix``."
  [& _args]
  ;; stub: original requires Matrix; pure-Clojure port not applicable
  (throw (ex-info "requires Matrix" {:type :UnsupportedOperation})))

(defn are-similar-matrices "Return ``True`` if ``matrix_a`` and ``matrix_b`` are similar matrices.\n\nParameters\n----------\nmatrix_a, matrix_b:\n    Square matrices represented as nested sequences (or SymPy matrices).\nsimplify_entries:\n    If ``True`` (default), the function attempts to simplify each entry so\n    that values that are algebraically equal are treated as such. Set this\n    to ``False`` to skip simplification when working with symbolic inputs\n    that should remain untouched.\n\nRaises\n------\nValueError\n    If the matrices are not square or their dimensions do not match.\nTypeError\n    If either matrix cannot be interpreted as a numeric matrix."
  [& _args]
  ;; stub: original requires MatrixError,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires MatrixError,numpy-attr:shape" {:type :UnsupportedOperation})))

(def --all-- ["are_similar_matrices"])

(def MatrixLike nil)

;; NOTE: _as_square_matrix stubbed (Matrix,nsimplify)
;; NOTE: _jordan_signature stubbed (Matrix)
;; NOTE: are_similar_matrices stubbed (MatrixError,numpy-attr:shape)
