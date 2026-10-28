(ns clojure-algorithms.neural-network.optimizers.momentum-sgd-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.optimizers.momentum-sgd :as m]))

(deftest momentum-sgd-test
  (testing "constructor stores momentum"
    (is (= 0.9 (:momentum (m/make-momentum-sgd 0.01 0.9)))))
  (testing "update keeps shape and descends"
    (let [o (m/make-momentum-sgd 0.1 0.9)
          [_ updated] (m/update o 0 [1.0 2.0] [0.1 0.2])]
      (is (= [0.99 1.98] updated))))
  (testing "momentum accumulates velocity across steps"
    (let [o (m/make-momentum-sgd 0.1 0.9)
          [o1 u1] (m/update o 0 [1.0] [1.0])
          [_ u2] (m/update o1 0 u1 [1.0])]
      (is (< (first u2) (first u1)))))
  (testing "minimizing f(x) = x^2 converges downward"
    (let [[_ x] (reduce (fn [[st x] _] (m/update st 0 x [(* 2.0 (first x))]))
                        [(m/make-momentum-sgd 0.1 0.9) [5.0]]
                        (range 20))]
      (is (< (Math/abs (first x)) 5.0)))))
