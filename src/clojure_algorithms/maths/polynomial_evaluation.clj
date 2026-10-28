(ns clojure-algorithms.maths.polynomial-evaluation
  "Port of maths/polynomial_evaluation.py" (:require [clojure-algorithms.runtime.python :refer :all]))

(declare evaluate-poly horner)

(defn evaluate-poly "Evaluate a polynomial f(x) at specified point x and return the value.\n\nArguments:\npoly -- the coefficients of a polynomial as an iterable in order of\n        ascending degree\nx -- the point at which to evaluate the polynomial\n\n>>> evaluate_poly((0.0, 0.0, 5.0, 9.3, 7.0), 10.0)\n79800.0"
  [poly x] (reduce +' 0 (doall (for [[i c] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) poly))] (py-mul c (py-pow x i))))))

(defn horner "Evaluate a polynomial at specified point using Horner's method.\n\nIn terms of computational complexity, Horner's method is an efficient method\nof evaluating a polynomial. It avoids the use of expensive exponentiation,\nand instead uses only multiplication and addition to evaluate the polynomial\nin O(n), where n is the degree of the polynomial.\n\nhttps://en.wikipedia.org/wiki/Horner's_method\n\nArguments:\npoly -- the coefficients of a polynomial as an iterable in order of\n        ascending degree\nx -- the point at which to evaluate the polynomial\n\n>>> horner((0.0, 0.0, 5.0, 9.3, 7.0), 10.0)\n79800.0"
  [poly x] (let [coeff (atom nil) result (atom nil)] (try (reset! result 0.0) (doseq [__i1 (py-iter (vec (reverse poly)))] (reset! coeff __i1) (reset! result (py-add (py-mul @result x) @coeff))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @result})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
