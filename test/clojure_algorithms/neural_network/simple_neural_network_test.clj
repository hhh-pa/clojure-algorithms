(ns clojure-algorithms.neural-network.simple-neural-network-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.simple-neural-network :refer [sigmoid-function forward-propagation]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest simple-nn-test
  (testing "sigmoid doctests"
    (is (approx= 0.9706877692486436 (sigmoid-function 3.5) 1e-12))
    (is (approx= -8.75 (sigmoid-function 3.5 true) 1e-12)))
  (testing "long training converges near expected"
    (let [res (forward-propagation 32 450000 (java.util.Random. 42))]
      (is (and (> res 31) (< res 33)))))
  (testing "short training has not converged"
    (let [res (forward-propagation 32 1000 (java.util.Random. 42))]
      (is (not (and (> res 31) (< res 33)))))))
