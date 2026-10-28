(ns clojure-algorithms.neural-network.activation-functions.swish-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.swish :refer [sigmoid sigmoid-linear-unit swish]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest swish-test
  (testing "sigmoid doctest"
    (is (approx-vec= [0.26894142 0.73105858 0.88079708]
                     (sigmoid [-1.0 1.0 2.0]) 1e-6)))
  (testing "silu doctests"
    (is (approx-vec= [-0.26894142 0.73105858 1.76159416]
                     (sigmoid-linear-unit [-1.0 1.0 2.0]) 1e-6))
    (is (approx-vec= [-0.23840584] (sigmoid-linear-unit [-2]) 1e-6)))
  (testing "swish doctests"
    (is (approx-vec= [-0.11920292 0.88079708 1.96402758]
                     (swish [-1.0 1.0 2.0] 2) 1e-6))
    (is (approx-vec= [-0.23840584] (swish [-2] 1) 1e-6))))
