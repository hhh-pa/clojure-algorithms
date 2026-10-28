(ns clojure-algorithms.neural-network.back-propagation-neural-network-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.back-propagation-neural-network :as bp]))

(deftest bpnn-test
  (testing "sigmoid sanity"
    (is (= [0.5] (bp/sigmoid [0.0]))))
  (testing "cal-loss returns squared error and gradient"
    (let [[loss grad] (bp/cal-loss [1.0 2.0] [1.5 2.5])]
      (is (< (Math/abs (- 0.5 loss)) 1e-9))
      (is (= [1.0 1.0] grad))))
  (testing "forward then backward preserves shapes"
    (let [rng (java.util.Random. 3)
          layer (bp/init-layer (bp/make-dense-layer 2) 3 rng)
          [layer' out] (bp/layer-forward layer [0.5 -0.5 1.0])
          [layer'' grad] (bp/layer-backward layer' [0.1 -0.2])]
      (is (= 2 (count out)))
      (is (= 3 (count grad)))
      (is (= [2 3] [(count (:weights layer'')) (count (first (:weights layer'')))]))))
  (testing "build marks first layer as input"
    (let [net (bp/build-network
               (reduce bp/add-layer (bp/make-bpnn) [(bp/make-dense-layer 3) (bp/make-dense-layer 2)])
               (java.util.Random. 3))]
      (is (true? (:input-layer? (first (:layers net)))))
      (is (= [2 3] [(count (:weights (second (:layers net))))
                    (count (first (:weights (second (:layers net)))))]))))
  (testing "training records loss history"
    (let [rng (java.util.Random. 3)
          net (bp/build-network
               (reduce bp/add-layer (bp/make-bpnn) [(bp/make-dense-layer 2) (bp/make-dense-layer 1)])
               rng)
          [trained _] (bp/train net [[0.5 0.5] [0.1 0.9]] [[0.8] [0.2]] 3 1e-9)]
      (is (= 3 (count (:train-mse trained))))
      (is (every? #(Double/isFinite (double %)) (:train-mse trained))))))
