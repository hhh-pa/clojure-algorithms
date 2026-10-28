(ns clojure-algorithms.neural-network.activation-functions.softplus-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.softplus :refer [softplus]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest softplus-test
  (testing "doctest examples"
    (is (approx-vec= [2.39554546 1.03748795 0.12692801 0.02212422]
                     (softplus [2.3 0.6 -2 -3.8]) 1e-6))
    (is (approx-vec= [1.01034298e-04 5.54355244e-01 9.43248946e-01 1.04077103e-02]
                     (softplus [-9.2 -0.3 0.45 -4.56]) 1e-6)))
  (testing "zero maps to ln2"
    (is (approx= (Math/log 2.0) (first (softplus [0])) 1e-12))))
