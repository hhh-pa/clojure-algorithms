(ns clojure-algorithms.maths.fibonacci-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.fibonacci :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/time-func))
  (is (some? #'sut/fib-iterative-yield))
  (is (some? #'sut/fib-iterative))
  (is (some? #'sut/fib-recursive))
  (is (some? #'sut/fib-recursive-cached))
  (is (some? #'sut/fib-memoization))
  (is (some? #'sut/fib-binet))
  (is (some? #'sut/matrix-pow-np))
  (is (some? #'sut/fib-matrix-np)))

(deftest test-fib-values
  (testing "upstream doctest values"
    (is (= [0] (sut/fib-iterative 0)))
    (is (= [0 1 1 2 3 5] (sut/fib-iterative 5)))
    (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-iterative 10)))
    (is (= [0 1 1 2 3 5] (sut/fib-iterative-yield 5)))
    (is (= [0 1 1 2 3 5] (sut/fib-recursive 5)))
    (is (= [0 1 1 2 3 5] (sut/fib-recursive-cached 5)))
    (is (= [0] (sut/fib-memoization 0)))
    (is (= [0 1 1 2 3 5] (sut/fib-memoization 5)))
    (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-memoization 10)))
    (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-binet 10))))
  (testing "negative input raises"
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-iterative -1)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-memoization -1)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-binet -1))))
  (testing "documented stubs raise UnsupportedOperation"
    (is (thrown? clojure.lang.ExceptionInfo (sut/time-func)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/matrix-pow-np)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-matrix-np)))))
