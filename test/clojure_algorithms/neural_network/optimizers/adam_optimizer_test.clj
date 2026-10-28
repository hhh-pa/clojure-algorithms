(ns clojure-algorithms.neural-network.optimizers.adam-optimizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.optimizers.adam-optimizer :as a]))

(deftest adam-test
  (testing "constructor stores hyper-parameters"
    (let [o (a/make-adam 0.001 0.9 0.999)]
      (is (= 0.9 (:beta1 o)))))
  (testing "update keeps shape and descends"
    (let [o (a/make-adam 0.1)
          [_ updated] (a/update o 0 [1.0 2.0] [0.1 0.2])]
      (is (= 2 (count updated)))
      (is (< (first updated) 1.0))))
  (testing "minimizing f(x) = x^2 converges downward"
    (let [[_ x] (reduce (fn [[st x] _] (a/update st 0 x [(* 2.0 (first x))]))
                        [(a/make-adam 0.1) [5.0]]
                        (range 20))]
      (is (< (Math/abs (first x)) 5.0))))
  (testing "independent state per param-id"
    (let [o (a/make-adam 0.1)
          [o1 _] (a/update o 0 [1.0] [0.5])
          [_ u2] (a/update o1 1 [1.0] [0.5])]
      (is (= 1 (count u2))))))
