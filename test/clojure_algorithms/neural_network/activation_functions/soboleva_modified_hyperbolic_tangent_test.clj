(ns clojure-algorithms.neural-network.activation-functions.soboleva-modified-hyperbolic-tangent-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.soboleva-modified-hyperbolic-tangent :refer [soboleva-modified-hyperbolic-tangent]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest smht-test
  (testing "doctest example"
    (is (approx-vec= [0.11075085 -0.28236685 0.07861169 -0.1180085 0.22999056 0.1566043]
                     (soboleva-modified-hyperbolic-tangent [5.4 -2.4 6.3 -5.23 3.27 0.56]
                                                           0.2 0.4 0.6 0.8)
                     1e-6)))
  (testing "zero maps to zero"
    (is (approx-vec= [0.0]
                     (soboleva-modified-hyperbolic-tangent [0] 0.2 0.4 0.6 0.8)
                     1e-12))))
