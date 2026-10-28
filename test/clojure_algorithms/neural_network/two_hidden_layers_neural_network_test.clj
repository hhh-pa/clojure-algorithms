(ns clojure-algorithms.neural-network.two-hidden-layers-neural-network-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.two-hidden-layers-neural-network :as nn]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest two-hidden-layers-test
  (testing "sigmoid doctest"
    (let [r (nn/sigmoid [[1.0 0.0 2.0] [1.0 0.0 0.0]])]
      (is (approx= 0.73105858 (get-in r [0 0]) 1e-6))
      (is (approx= 0.5 (get-in r [0 1]) 1e-9))
      (is (approx= 0.88079708 (get-in r [0 2]) 1e-6))))
  (testing "sigmoid-derivative doctest"
    (let [r (nn/sigmoid-derivative [[1.0 0.0 2.0] [1.0 0.0 0.0]])]
      (is (= [[0.0 0.0 -2.0] [0.0 0.0 0.0]] r))))
  (testing "feedforward returns finite output"
    (let [net (nn/make-network [[0.0 0.0 0.0] [0.0 0.0 0.0] [0.0 0.0 0.0]]
                               [[0.0] [0.0] [0.0]]
                               (java.util.Random. 7))
          [_ out] (nn/feedforward net)]
      (is (every? #(Double/isFinite (double %)) (flatten out)))))
  (testing "back-propagation changes weights"
    (let [net (nn/make-network [[0.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]]
                               [[0.0] [1.0] [1.0]]
                               (java.util.Random. 7))
          [net2 _] (nn/feedforward net)
          before (:w3 net2)
          after (:w3 (nn/back-propagation net2))]
      (is (not= before after))))
  (testing "training then predicting yields a class"
    (let [net (nn/make-network [[0.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]]
                               [[0.0] [1.0] [1.0]]
                               (java.util.Random. 7))
          trained (nn/train net 50 false)]
      (is (contains? #{0 1} (nn/predict trained [0 1 0])))))
  (testing "example returns a class"
    (is (contains? #{0 1} (nn/example)))))
