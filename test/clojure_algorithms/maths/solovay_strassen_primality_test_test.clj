(ns clojure-algorithms.maths.solovay-strassen-primality-test-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.solovay-strassen-primality-test :as sut]))

(deftest test-fn-jacobi-symbol-examples
  (is (approx -1 (sut/jacobi-symbol 2 13)))
  (is (approx 1 (sut/jacobi-symbol 5 19)))
  (is (approx 0 (sut/jacobi-symbol 7 14))))

(deftest test-fn-solovay-strassen-examples
  (is (= true (boolean (sut/solovay-strassen 13 5))))
  (is (= false (boolean (sut/solovay-strassen 9 10))))
  (is (= true (boolean (sut/solovay-strassen 17 15)))))

(deftest test-fn-solovay-strassen-extra
  (is (= false (boolean (sut/solovay-strassen 0 5))))
  (is (= false (boolean (sut/solovay-strassen 1 5))))
  (is (= true (boolean (sut/solovay-strassen 2 5))))
  (is (= true (boolean (sut/solovay-strassen 3 5))))
  (is (= 0 (sut/jacobi-symbol 0 13)))
  (is (= 1 (sut/jacobi-symbol 1 13))))
