(ns clojure-algorithms.neural-network.activation-functions.softsign-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.softsign :refer [softsign]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest softsign-test
  (testing "doctest example"
    (is (approx-vec= [-0.83333333 -0.5 0.0 0.5 0.83333333]
                     (softsign [-5 -1 0 1 5]) 1e-6)))
  (testing "output bounded in (-1, 1)"
    (is (every? #(< -1.0 % 1.0) (softsign [-1000 1000])))))
