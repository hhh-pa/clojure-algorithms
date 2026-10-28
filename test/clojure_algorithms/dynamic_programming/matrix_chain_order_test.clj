(ns clojure-algorithms.dynamic-programming.matrix-chain-order-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.matrix-chain-order :as mc]))

(deftest matrix-chain-order-test
  (testing "cost and split tables"
    (is (= [[[0 0 0] [0 0 1500] [0 0 0]]
            [[0 0 0] [0 0 1] [0 0 0]]]
           (mc/matrix-chain-order [10 30 5]))))
  (testing "classic example costs 15125"
    (let [[cost sol] (mc/matrix-chain-order [30 35 15 5 10 20 25])]
      (is (= 15125 (get-in cost [1 6])))
      (is (= "( ( A1 ( A2 A3 ) ) ( ( A4 A5 ) A6 ) ) "
             (mc/optimal-solution sol 1 6)))))
  (testing "printing matches the string form"
    (let [[_ sol] (mc/matrix-chain-order [10 30 5])]
      (is (= "( A1 A2 ) " (with-out-str (mc/print-optimal-solution sol 1 2)))))))
