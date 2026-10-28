(ns clojure-algorithms.maths.polynomial-evaluation
  "Port of maths/polynomial_evaluation.py.")

(defn- ipow
  "Integer exponentiation, preserving exact numeric types."
  [x i]
  (reduce *' 1 (repeat i x)))

(defn evaluate-poly
  "Evaluates a polynomial f(x) at the specified point x and returns the value.

  Arguments:
  poly -- the coefficients of a polynomial as a sequential collection
          in order of ascending degree
  x -- the point at which to evaluate the polynomial"
  [poly x]
  (reduce +' 0 (map-indexed (fn [i c] (*' c (ipow x i))) poly)))

(defn horner
  "Evaluates a polynomial at the specified point using Horner's method.

  In terms of computational complexity, Horner's method is an efficient method
  of evaluating a polynomial. It avoids the use of expensive exponentiation,
  and instead uses only multiplication and addition to evaluate the polynomial
  in O(n), where n is the degree of the polynomial.

  https://en.wikipedia.org/wiki/Horner's_method

  Arguments:
  poly -- the coefficients of a polynomial as a sequential collection
          in order of ascending degree
  x -- the point at which to evaluate the polynomial"
  [poly x]
  (reduce (fn [result coeff] (+ (* result x) coeff))
          0.0
          (reverse poly)))
