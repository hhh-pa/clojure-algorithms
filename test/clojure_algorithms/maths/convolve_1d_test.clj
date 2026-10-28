(ns clojure-algorithms.maths.convolve-1d-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.convolve-1d :as sut]))

(deftest test-class-signal-examples
  (let [s (sut/make-Signal [1.0 3.0 2.0 -1.0])]
    (is (approx [1.0 3.0 2.0 -1.0] (:signal s)))
    (is (= 4 (:n s))))
  (is (thrown? clojure.lang.ExceptionInfo (sut/make-Signal [1.0 "x"]))))

(deftest test-class-discreteconvolve1d-examples
  (let [s1 (sut/make-Signal [1 2 3 4 5])
        s2 (sut/make-Signal [1 -1 2 -3])
        dc (sut/make-DiscreteConvolve1D s1 s2)]
    (is (= s1 (:kern dc)))
    (is (= s2 (:sig dc)))
    (let [conv (sut/DiscreteConvolve1D-convolve-1d dc)]
      (is (approx [9.0 -8.0 -7.0 -6.0] (:signal conv)))
      (is (= 4 (:n conv))))))
