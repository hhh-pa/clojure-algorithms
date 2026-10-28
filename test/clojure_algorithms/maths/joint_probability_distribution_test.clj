(ns clojure-algorithms.maths.joint-probability-distribution-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.joint-probability-distribution :as sut]))

(deftest test-fn-joint-probability-distribution-examples
  (let [dist (sut/joint-probability-distribution [1 2] [-2 5 8] [0.7 0.3] [0.3 0.5 0.2])]
    (is (approx 0.14 (get dist [1 8])))
    (is (approx {[1 -2] 0.21 [1 5] 0.35 [1 8] 0.14
                 [2 -2] 0.09 [2 5] 0.15 [2 8] 0.06}
                dist))
    (is (approx {[1 -2] 0.21 [1 5] 0.35 [2 -2] 0.09 [2 5] 0.15 [2 8] 0.06}
                (dissoc dist [1 8])))))

(deftest test-fn-expectation-examples
  (is (approx 1.3 (sut/expectation [1 2] [0.7 0.3]))))

(deftest test-fn-variance-examples
  (is (approx 0.21 (sut/variance [1 2] [0.7 0.3]))))

(deftest test-fn-covariance-examples
  (is (approx -5.551115123125783e-17
              (sut/covariance [1 2] [-2 5 8] [0.7 0.3] [0.3 0.5 0.2])))
  (is (approx 0.0 (sut/covariance [1 2] [-2 5 8] [0.7 0.3] [0.3 0.5 0.2]))))

(deftest test-fn-standard-deviation-examples
  (is (approx 0.458257569495584 (sut/standard-deviation 0.21)))
  (is (approx 2.0 (sut/standard-deviation 4.0))))
