(ns clojure-algorithms.neural-network.optimizers.nesterov-accelerated-sgd-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.optimizers.nesterov-accelerated-sgd :as n]))

(deftest nesterov-test
  (testing "constructor stores momentum"
    (is (= 0.9 (:momentum (n/make-nesterov 0.01 0.9)))))
  (testing "update keeps shape and descends"
    (let [o (n/make-nesterov 0.1 0.9)
          [_ updated] (n/update o 0 [1.0 2.0] [0.1 0.2])]
      (is (= 2 (count updated)))
      (is (< (first updated) 1.0))))
  (testing "minimizing f(x) = x^2 converges downward"
    (let [[_ x] (reduce (fn [[st x] _] (n/update st 0 x [(* 2.0 (first x))]))
                        [(n/make-nesterov 0.1 0.9) [5.0]]
                        (range 20))]
      (is (< (Math/abs (first x)) 5.0)))))
