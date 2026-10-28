(ns clojure-algorithms.neural-network.activation-functions.scaled-exponential-linear-unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.scaled-exponential-linear-unit :refer [scaled-exponential-linear-unit]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest selu-test
  (testing "doctest examples"
    (is (approx-vec= [1.36591 3.88759 2.52168]
                     (scaled-exponential-linear-unit [1.3 3.7 2.4]) 1e-4))
    (is (approx-vec= [1.36591 4.93829 8.61574]
                     (scaled-exponential-linear-unit [1.3 4.7 8.2]) 1e-4)))
  (testing "negative branch uses alpha"
    (let [[v] (scaled-exponential-linear-unit [-1.0] 1.6732 1.0507)]
      (is (approx= (* 1.0507 1.6732 (- (Math/exp -1.0) 1)) v 1e-9))))
  (testing "zero maps to zero"
    (is (approx-vec= [0.0] (scaled-exponential-linear-unit [0]) 1e-12))))
