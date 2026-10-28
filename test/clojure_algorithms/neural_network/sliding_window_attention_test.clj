(ns clojure-algorithms.neural-network.sliding-window-attention-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.sliding-window-attention :as swa]))

(deftest sliding-window-test
  (testing "output shape matches input shape"
    (let [rng (java.util.Random. 1)
          att (swa/make-attention 4 3 rng)
          x (vec (for [_ (range 2)]
                   (vec (for [_ (range 10)]
                          (vec (for [_ (range 4)] (.nextGaussian rng)))))))
          out (swa/forward att x)]
      (is (= [2 10 4] [(count out) (count (first out)) (count (first (first out)))]))
      (is (not (every? zero? (flatten out))))))
  (testing "window-size 1 projects each frame independently"
    (let [att {:embed-dim 2 :window-size 1
               :attention-weights [[1.0 0.0] [0.0 1.0]]}
          out (swa/forward att [[[2.0 3.0]]])]
      (is (= [[[2.0 3.0]]] out)))))
