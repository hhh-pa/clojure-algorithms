(ns clojure-algorithms.bit-manipulation.binary-count-setbits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-count-setbits :refer [binary-count-setbits]]))

(deftest binary-count-setbits-test
  (testing "doctest examples"
    (is (= 3 (binary-count-setbits 25)))
    (is (= 2 (binary-count-setbits 36)))
    (is (= 1 (binary-count-setbits 16)))
    (is (= 4 (binary-count-setbits 58)))
    (is (= 32 (binary-count-setbits 4294967295)))
    (is (= 0 (binary-count-setbits 0))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-setbits -10)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-setbits 0.8)))))
