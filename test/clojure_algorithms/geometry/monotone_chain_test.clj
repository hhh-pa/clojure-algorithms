(ns clojure-algorithms.geometry.monotone-chain-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.monotone-chain
             :refer [->Point cross-product-direction monotone-chain]]))

(deftest test-cross-product-direction
  (is (= 1 (cross-product-direction (->Point 0 0) (->Point 1 0) (->Point 1 1))))
  (is (= -1 (cross-product-direction (->Point 0 0) (->Point 1 1) (->Point 1 0))))
  (is (= 0 (cross-product-direction (->Point 0 0) (->Point 1 1) (->Point 2 2)))))

(deftest test-monotone-chain
  (is (= [] (monotone-chain [])))
  (is (= [(->Point 1 1)] (monotone-chain [(->Point 1 1)])))
  (is (= [(->Point 0 0) (->Point 2 0) (->Point 2 2) (->Point 0 2)]
         (monotone-chain [(->Point 0 0) (->Point 2 0) (->Point 2 2)
                          (->Point 0 2) (->Point 1 1) (->Point 1 0.5)])))
  (is (= [(->Point 0 0) (->Point 3 3)]
         (monotone-chain [(->Point 0 0) (->Point 1 1)
                          (->Point 2 2) (->Point 3 3)]))))
