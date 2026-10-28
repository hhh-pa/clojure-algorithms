(ns clojure-algorithms.neural-network.perceptron-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.perceptron :as p]))

(deftest perceptron-test
  (testing "constructor validation"
    (is (thrown? clojure.lang.ExceptionInfo (p/make-perceptron [] [0 1 2])))
    (is (thrown? clojure.lang.ExceptionInfo (p/make-perceptron [[0] [1] [2]] [])))
    (is (thrown? clojure.lang.ExceptionInfo (p/make-perceptron [[0] [1] [2]] [0 1]))))
  (testing "sign function"
    (is (= 1 (p/sign-fn 0)))
    (is (= -1 (p/sign-fn -0.5)))
    (is (= 1 (p/sign-fn 0.5))))
  (testing "single-sample training terminates and classifies"
    (let [[trained epochs] (p/train (p/make-perceptron [[2.0149 0.6192 10.9263]] [-1]))]
      (is (pos? epochs))
      (is (= -1 (p/classify trained [2.0149 0.6192 10.9263])))))
  (testing "classify before training throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (p/classify (p/make-perceptron [[0]] [1]) [0]))))
  (testing "full dataset trains within epoch cap"
    (let [[trained epochs] (p/train (p/make-perceptron p/samples p/targets))]
      (is (<= epochs 1000))
      (is (every? #{-1 1} (map #(p/classify trained %) p/samples))))))
