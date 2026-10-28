(ns clojure-algorithms.maths.matrix-exponentiation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.matrix-exponentiation :as sut]))

(deftest test-fn-fibonacci-with-matrix-exponentiation-examples
  (is (approx 5 (sut/fibonacci-with-matrix-exponentiation 1 5 6)))
  (is (approx 11 (sut/fibonacci-with-matrix-exponentiation 2 10 11)))
  (is (approx 144 (sut/fibonacci-with-matrix-exponentiation 13 0 1)))
  (is (approx 411 (sut/fibonacci-with-matrix-exponentiation 10 5 9)))
  (is (approx 89 (sut/fibonacci-with-matrix-exponentiation 9 2 3))))

(deftest test-fn-simple-fibonacci-examples
  (is (approx 5 (sut/simple-fibonacci 1 5 6)))
  (is (approx 11 (sut/simple-fibonacci 2 10 11)))
  (is (approx 144 (sut/simple-fibonacci 13 0 1)))
  (is (approx 411 (sut/simple-fibonacci 10 5 9)))
  (is (approx 89 (sut/simple-fibonacci 9 2 3))))

(deftest test-matrix-ops
  (testing "zero matrix and multiplication"
    (is (= [[0 0] [0 0]] (:t (sut/make-Matrix 2))))
    (is (= [[1 2] [3 4]] (:t (sut/make-Matrix [[1 2] [3 4]]))))
    (is (= [[19 22] [43 50]]
           (:t (sut/Matrix-mul (sut/make-Matrix [[1 2] [3 4]])
                               (sut/make-Matrix [[5 6] [7 8]]))))))
  (testing "benchmarks return a non-negative elapsed time"
    (is (number? (sut/matrix-exponentiation-time)))
    (is (number? (sut/simple-fibonacci-time)))))
