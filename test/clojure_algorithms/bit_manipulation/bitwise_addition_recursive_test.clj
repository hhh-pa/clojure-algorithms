(ns clojure-algorithms.bit-manipulation.bitwise-addition-recursive-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.bitwise-addition-recursive
             :refer [bitwise-addition-recursive]]))

(deftest bitwise-addition-recursive-test
  (testing "doctest examples"
    (is (= 9 (bitwise-addition-recursive 4 5)))
    (is (= 17 (bitwise-addition-recursive 8 9)))
    (is (= 4 (bitwise-addition-recursive 0 4)))
    (is (= 300 (bitwise-addition-recursive 123 177))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (bitwise-addition-recursive 4.5 9)))
    (is (thrown? clojure.lang.ExceptionInfo (bitwise-addition-recursive "4" 9)))
    (is (thrown? clojure.lang.ExceptionInfo (bitwise-addition-recursive "4.5" 9)))
    (is (thrown? clojure.lang.ExceptionInfo (bitwise-addition-recursive -1 9)))
    (is (thrown? clojure.lang.ExceptionInfo (bitwise-addition-recursive 1 -9)))))
