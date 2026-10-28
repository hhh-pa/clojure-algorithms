(ns clojure-algorithms.neural-network.activation-functions.exponential-linear-unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.exponential-linear-unit :refer [exponential-linear-unit elu-value]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest elu-test
  (testing "doctest examples"
    (is (approx-vec= [2.3 0.6 -0.25939942 -0.29328877]
                     (exponential-linear-unit [2.3 0.6 -2 -3.8] 0.3) 1e-6))
    (is (approx-vec= [-0.06699323 -0.01736518 0.45 -0.06629904]
                     (exponential-linear-unit [-9.2 -0.3 0.45 -4.56] 0.067) 1e-6)))
  (testing "positive values pass through"
    (is (= 2.5 (elu-value 2.5 1.0))))
  (testing "zero maps to zero"
    (is (approx= 0.0 (elu-value 0 0.5) 1e-12))))
