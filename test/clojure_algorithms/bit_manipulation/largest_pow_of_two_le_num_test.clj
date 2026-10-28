(ns clojure-algorithms.bit-manipulation.largest-pow-of-two-le-num-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.largest-pow-of-two-le-num
             :refer [largest-pow-of-two-le-num]]))

(deftest largest-pow-of-two-le-num-test
  (testing "doctest examples"
    (is (= 0 (largest-pow-of-two-le-num 0)))
    (is (= 1 (largest-pow-of-two-le-num 1)))
    (is (= 0 (largest-pow-of-two-le-num -1)))
    (is (= 2 (largest-pow-of-two-le-num 3)))
    (is (= 8 (largest-pow-of-two-le-num 15)))
    (is (= 64 (largest-pow-of-two-le-num 99)))
    (is (= 128 (largest-pow-of-two-le-num 178)))
    (is (= 524288 (largest-pow-of-two-le-num 999999))))
  (testing "float input raises"
    (is (thrown? clojure.lang.ExceptionInfo (largest-pow-of-two-le-num 99.9)))))
