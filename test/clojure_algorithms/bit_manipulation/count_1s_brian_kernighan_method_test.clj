(ns clojure-algorithms.bit-manipulation.count-1s-brian-kernighan-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.count-1s-brian-kernighan-method
             :refer [get-1s-count]]))

(deftest get-1s-count-test
  (testing "doctest examples"
    (is (= 3 (get-1s-count 25)))
    (is (= 3 (get-1s-count 37)))
    (is (= 3 (get-1s-count 21)))
    (is (= 4 (get-1s-count 58)))
    (is (= 0 (get-1s-count 0)))
    (is (= 1 (get-1s-count 256))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (get-1s-count -1)))
    (is (thrown? clojure.lang.ExceptionInfo (get-1s-count 0.8)))
    (is (thrown? clojure.lang.ExceptionInfo (get-1s-count "25")))))
