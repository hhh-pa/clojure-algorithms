(ns clojure-algorithms.neural-network.activation-functions.mish-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.mish :refer [mish]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest mish-test
  (testing "doctest examples"
    (is (approx-vec= [2.26211893 0.46613649 -0.25250148 -0.08405831]
                     (mish [2.3 0.6 -2 -3.8]) 1e-6))
    (is (approx-vec= [-0.00092952 -0.15113318 0.33152014 -0.04745745]
                     (mish [-9.2 -0.3 0.45 -4.56]) 1e-6)))
  (testing "zero maps to zero"
    (is (approx-vec= [0.0] (mish [0]) 1e-12))))
