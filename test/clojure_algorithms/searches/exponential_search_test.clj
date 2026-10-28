(ns clojure-algorithms.searches.exponential-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.exponential-search :as es]))

(deftest binary-search-by-recursion-test
  (testing "bounded recursive binary search"
    (is (= 0 (es/binary-search-by-recursion [0 5 7 10 15] 0 0 4)))
    (is (= 4 (es/binary-search-by-recursion [0 5 7 10 15] 15 0 4)))
    (is (= 1 (es/binary-search-by-recursion [0 5 7 10 15] 5 0 4)))
    (is (= -1 (es/binary-search-by-recursion [0 5 7 10 15] 6 0 4)))
    (is (= -1 (es/binary-search-by-recursion [0 5 7 10 15] -1)))
    (is (= -1 (es/binary-search-by-recursion [0 5 7 10 15] 16)))))

(deftest exponential-search-test
  (testing "exponential search"
    (is (= 0 (es/exponential-search [0 5 7 10 15] 0)))
    (is (= 4 (es/exponential-search [0 5 7 10 15] 15)))
    (is (= 1 (es/exponential-search [0 5 7 10 15] 5)))
    (is (= -1 (es/exponential-search [0 5 7 10 15] 6)))
    (is (= -1 (es/exponential-search [0 5 7 10 15] -3)))
    (is (= -1 (es/exponential-search [0 5 7 10 15] 20)))
    (is (= -1 (es/exponential-search [] 1))))
  (testing "unsorted input throws"
    (is (thrown? clojure.lang.ExceptionInfo (es/exponential-search [2 1] 1)))))
