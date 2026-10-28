(ns clojure-algorithms.dynamic-programming.matrix-chain-multiplication-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.matrix-chain-multiplication :as mc]))

(deftest matrix-chain-test
  (testing "bottom-up"
    (is (= 30 (mc/matrix-chain-multiply [1 2 3 4 3])))
    (is (= 0 (mc/matrix-chain-multiply [10])))
    (is (= 0 (mc/matrix-chain-multiply [10 20])))
    (is (= 722 (mc/matrix-chain-multiply [19 2 19])))
    (is (= 323398 (mc/matrix-chain-multiply (vec (range 1 100))))))
  (testing "memoized"
    (is (= 30 (mc/matrix-chain-order [1 2 3 4 3])))
    (is (= 0 (mc/matrix-chain-order [10])))
    (is (= 0 (mc/matrix-chain-order [10 20])))
    (is (= 722 (mc/matrix-chain-order [19 2 19])))
    (is (= 323398 (mc/matrix-chain-order (vec (range 1 100)))))))
