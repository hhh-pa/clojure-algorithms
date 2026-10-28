(ns clojure-algorithms.maths.simultaneous-linear-equation-solver
  "https://en.wikipedia.org/wiki/Augmented_matrix\n\nThis algorithm solves simultaneous linear equations of the form\nλa + λb + λc + λd + ... = y as [λ, λ, λ, λ, ..., y]\nWhere λ & y are individual coefficients, the no. of equations = no. of coefficients - 1\n\nNote in order to work there must exist 1 equation where all instances of λ and y != 0")

(declare simplify solve-simultaneous)

(defn simplify ">>> simplify([[1, 2, 3], [4, 5, 6]])\n[[1.0, 2.0, 3.0], [0.0, 0.75, 1.5]]\n>>> simplify([[5, 2, 5], [5, 1, 10]])\n[[1.0, 0.4, 1.0], [0.0, 0.2, -1.0]]"
  [& _args]
  ;; stub: original requires numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:copy" {:type :UnsupportedOperation})))

(defn solve-simultaneous ">>> solve_simultaneous([[1, 2, 3],[4, 5, 6]])\n[-1.0, 2.0]\n>>> solve_simultaneous([[0, -3, 1, 7],[3, 2, -1, 11],[5, 1, -2, 12]])\n[6.4, 1.2, 10.6]\n>>> solve_simultaneous([])\nTraceback (most recent call last):\n    ...\nIndexError: solve_simultaneous() requires n lists of length n+1\n>>> solve_simultaneous([[1, 2, 3],[1, 2]])\nTraceback (most recent call last):\n    ...\nIndexError: solve_simultaneous() requires n lists of length n+1\n>>> solve_simultaneous([[1, 2, 3],[\"a\", 7, 8]])\nTraceback (most recent call last):\n    ...\nValueError: solve_simultaneous() requires lists of integers\n>>> solve_simultaneous([[0, 2, 3],[4, 0, 6]])\nTraceback (most recent call last):\n    ...\nValueError: solve_simultaneous() requires at least 1 full equation"
  [& _args]
  ;; stub: original requires numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:copy" {:type :UnsupportedOperation})))

;; NOTE: simplify stubbed (numpy-attr:copy)
;; NOTE: solve_simultaneous stubbed (numpy-attr:copy)
