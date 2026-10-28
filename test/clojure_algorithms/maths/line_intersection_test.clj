(ns clojure-algorithms.maths.line-intersection-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.line-intersection :as sut]))

(deftest test-fn-determinant-examples
  (is (approx 1 (sut/determinant 1 0 0 1)))
  (is (approx 0 (sut/determinant 1 2 1 2)))
  (is (approx -2 (sut/determinant 1 2 3 4)))
  (is (approx -200 (sut/determinant 10 20 30 40))))

(deftest test-fn-line-coefficients-examples
  (is (approx [1 0 1] (sut/line-coefficients [1 0] [1 2])))
  (is (approx [0.0 -1 1.0] (sut/line-coefficients [0 1] [2 1])))
  (is (approx [1.0 -1 0.0] (sut/line-coefficients [0 0] [1 1])))
  (is (approx [-1.0 -1 1.0] (sut/line-coefficients [0 1] [1 0]))))

(deftest test-fn-segment-intersection-examples
  (is (approx [0.5 0.5] (sut/segment-intersection [0 0] [1 1] [1 0] [0 1])))
  (is (nil? (sut/segment-intersection [0 0] [1 1] [2 2] [3 3])))
  (is (nil? (sut/segment-intersection [0 0] [0 1] [1 0] [1 1])))
  (is (approx [0.5 0.5]
              (sut/segment-intersection [0 0] [1 1] [1 0] [0 1] false)))
  (is (nil? (sut/segment-intersection [0 0] [1 1] [2 2] [3 3] false))))
