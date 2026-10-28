(ns clojure-algorithms.bit-manipulation.next-power-of-two-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.next-power-of-two :refer [next-power-of-two]]))

(deftest next-power-of-two-test
  (testing "doctest examples"
    (is (= 1 (next-power-of-two 1)))
    (is (= 2 (next-power-of-two 2)))
    (is (= 4 (next-power-of-two 3)))
    (is (= 8 (next-power-of-two 5)))
    (is (= 2048 (next-power-of-two 1025))))
  (testing "non-positive input raises"
    (is (thrown? clojure.lang.ExceptionInfo (next-power-of-two 0)))
    (is (thrown? clojure.lang.ExceptionInfo (next-power-of-two -3))))
  (testing "large integers"
    (let [two-to-99 (bigint (.shiftLeft java.math.BigInteger/ONE 99))
          two-to-100 (bigint (.shiftLeft java.math.BigInteger/ONE 100))]
      (is (= two-to-100 (next-power-of-two (inc two-to-99))))
      (is (= two-to-100 (next-power-of-two two-to-100))))))
