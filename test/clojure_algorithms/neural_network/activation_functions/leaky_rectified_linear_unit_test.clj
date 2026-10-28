(ns clojure-algorithms.neural-network.activation-functions.leaky-rectified-linear-unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.leaky-rectified-linear-unit :refer [leaky-rectified-linear-unit]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest leaky-relu-test
  (testing "doctest examples"
    (is (approx-vec= [2.3 0.6 -0.6 -1.14]
                     (leaky-rectified-linear-unit [2.3 0.6 -2 -3.8] 0.3) 1e-9))
    (is (approx-vec= [-0.6164 -0.0201 0.45 -0.30552]
                     (leaky-rectified-linear-unit [-9.2 -0.3 0.45 -4.56] 0.067) 1e-9)))
  (testing "zero maps to zero"
    (is (approx-vec= [0.0] (leaky-rectified-linear-unit [0] 0.1) 1e-12))))
