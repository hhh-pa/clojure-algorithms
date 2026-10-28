(ns clojure-algorithms.maths.monte-carlo-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.monte-carlo :as sut]))

(deftest test-fn-area-under-curve-estimator
  (testing "constant function integrates exactly in expectation"
    (let [est (sut/area-under-curve-estimator 200 (fn [_] 5) 12.0 14.0)]
      (is (< (Math/abs (- 10.0 est)) 1e-9))))
  (testing "identity over [0, 1] is near 0.5"
    (let [est (sut/area-under-curve-estimator 20000 identity)]
      (is (< (Math/abs (- 0.5 est)) 0.05))))
  (testing "default arities agree"
    (is (number? (sut/area-under-curve-estimator 200 identity 0.0)))
    (is (number? (sut/area-under-curve-estimator 200 identity)))))

(deftest test-printing-estimators
  (testing "reporters print and return nil"
    (is (nil? (sut/pi-estimator 200)))
    (is (nil? (sut/area-under-line-estimator-check 200)))
    (is (nil? (sut/pi-estimator-using-area-under-curve 200)))))
