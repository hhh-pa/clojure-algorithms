(ns clojure-algorithms.neural-network.optimizers.adagrad-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.optimizers.adagrad :as a]))

(deftest adagrad-test
  (testing "constructor defaults"
    (let [o (a/make-adagrad 0.01 1e-8)]
      (is (= 0.01 (:learning-rate o)))
      (is (= 1e-8 (:epsilon o)))))
  (testing "update keeps shape and descends"
    (let [o (a/make-adagrad 0.1)
          [_ updated] (a/update o 0 [1.0 2.0] [0.1 0.2])]
      (is (= 2 (count updated)))
      (is (< (first updated) 1.0))))
  (testing "minimizing f(x) = x^2 converges downward"
    (let [[_ x] (reduce (fn [[st x] _] (a/update st 0 x [(* 2.0 (first x))]))
                        [(a/make-adagrad 1.0) [5.0]]
                        (range 20))]
      (is (< (Math/abs (first x)) 5.0)))))
