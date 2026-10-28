(ns clojure-algorithms.neural-network.activation-functions.gaussian-error-linear-unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.gaussian-error-linear-unit :refer [sigmoid gaussian-error-linear-unit]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest gelu-test
  (testing "sigmoid doctest"
    (is (approx-vec= [0.26894142 0.73105858 0.88079708]
                     (sigmoid [-1.0 1.0 2.0]) 1e-6)))
  (testing "gelu doctests"
    (is (approx-vec= [-0.15420423 0.84579577 1.93565862]
                     (gaussian-error-linear-unit [-1.0 1.0 2.0]) 1e-6))
    (is (approx-vec= [-0.01807131]
                     (gaussian-error-linear-unit [-3]) 1e-6)))
  (testing "zero maps to zero"
    (is (approx-vec= [0.0] (gaussian-error-linear-unit [0]) 1e-12))))
