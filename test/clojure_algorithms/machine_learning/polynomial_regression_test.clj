(ns clojure-algorithms.machine-learning.polynomial-regression-test
  (:require [clojure-algorithms.machine-learning.polynomial-regression :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-constructor
  (is (= :ValueError (:type (try (sut/make-PolynomialRegression -1)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
  (let [model (sut/make-PolynomialRegression 3)]
    (is (= 3 (:degree model)))
    (is (nil? (:params model)))))

(deftest test-fn-design-matrix-examples
  (let [x [0 1 2]]
    (is (= [[1] [1] [1]] (sut/PolynomialRegression-design-matrix x 0)))
    (is (= [[1 0] [1 1] [1 2]]
           (sut/PolynomialRegression-design-matrix x 1)))
    (is (= [[1 0 0] [1 1 1] [1 2 4]]
           (sut/PolynomialRegression-design-matrix x 2)))
    (is (= [[1 0 0 0] [1 1 1 1] [1 2 4 8]]
           (sut/PolynomialRegression-design-matrix x 3))))
  (is (thrown? Exception (sut/PolynomialRegression-design-matrix
                          [[0 0] [0 0]] 3)))
  (is (= :ValueError
         (:type (try (sut/PolynomialRegression-design-matrix [[0 0] [0 0]] 3)
                     (catch clojure.lang.ExceptionInfo e (ex-data e))))))
  (testing "float inputs stay floats"
    (is (approx [[1.0 0.5 0.25]]
                (sut/PolynomialRegression-design-matrix [0.5] 2)))))

(deftest test-fn-predict-examples
  (testing "unfitted model raises like the upstream doctest"
    (is (= :ArithmeticError
           (:type (try (sut/PolynomialRegression-predict
                        (sut/make-PolynomialRegression 3) [0 1 2 3 4])
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
  (testing "prediction from fitted params matches the upstream doctests"
    ;; y = x^3 - 2x^2 + 3x - 5, params [-5 3 -2 1]
    (let [model (assoc (sut/make-PolynomialRegression 3)
                       :params [-5.0 3.0 -2.0 1.0])]
      (is (approx [-11.0] (sut/PolynomialRegression-predict model [-1])))
      (is (approx [-27.0] (sut/PolynomialRegression-predict model [-2])))
      (is (approx [157.0] (sut/PolynomialRegression-predict model [6]))))))

(deftest test-fn-stubs
  (is (= :UnsupportedOperation
         (:type (try (sut/PolynomialRegression-fit
                      (sut/make-PolynomialRegression 3) [0 1] [0 1])
                     (catch clojure.lang.ExceptionInfo e (ex-data e))))))
  (is (= :UnsupportedOperation
         (:type (try (sut/main)
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
