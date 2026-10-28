(ns clojure-algorithms.maths.numerical-analysis.newton-raphson-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.newton-raphson :as sut]))

(defn- ex-type
  [thunk]
  (try (thunk) :no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defn- isclose
  [a b tol]
  (<= (Math/abs (- (double a) (double b))) tol))

(deftest test-fn-calc-derivative-examples
  (is (isclose (sut/calc-derivative (fn [x] (* x x)) 2) 4 1e-5))
  (is (isclose (sut/calc-derivative #(Math/sin %) 0) 1 1e-5)))

(deftest test-fn-newton-raphson-examples
  (let [[root _ _] (sut/newton-raphson (fn [x] (+ (* x x) (* -5 x) 2))
                                       0.4 100 1e-6 1e-15)]
    (is (isclose root (/ (- 5 (Math/sqrt 17)) 2) 1e-15)))
  (let [[root _ _] (sut/newton-raphson (fn [x] (- (Math/log (double x)) 1))
                                       2 100 1e-6 1e-15)]
    (is (isclose root Math/E 1e-15)))
  (let [[root _ _] (sut/newton-raphson #(Math/sin %) 1 100 1e-6 1e-15)]
    (is (isclose root 0 1e-15)))
  (is (= :ZeroDivisionError
         (ex-type #(sut/newton-raphson (fn [x] (Math/cos (double x))) 0))))
  (is (= :ArithmeticError
         (ex-type #(sut/newton-raphson (fn [x] (+ (* x x) 1)) 2)))))

(deftest test-newton-raphson-steps
  (let [[_ _ steps] (sut/newton-raphson (fn [x] (+ (* x x) (* -5 x) 2)) 0.4)]
    (is (= [] steps)))
  (let [[root error steps] (sut/newton-raphson (fn [x] (+ (* x x) (* -5 x) 2))
                                               0.4 100 1e-6 1e-6 true)]
    (is (approx 0.4384471871911697 root))
    (is (< error 1e-6))
    (is (= 0.4 (first steps)))
    (is (pos? (count steps)))))
