(ns clojure-algorithms.maths.fibonacci-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.fibonacci :as sut]))

(deftest test-fn-fib-iterative-yield-examples
  (is (= [0] (sut/fib-iterative-yield 0)))
  (is (= [0 1] (sut/fib-iterative-yield 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-iterative-yield 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-iterative-yield 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-iterative-yield -1))))

(deftest test-fn-fib-iterative-examples
  (is (= [0] (sut/fib-iterative 0)))
  (is (= [0 1] (sut/fib-iterative 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-iterative 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-iterative 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-iterative -1))))

(deftest test-fn-fib-recursive-examples
  (is (= [0] (sut/fib-recursive 0)))
  (is (= [0 1] (sut/fib-recursive 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-recursive 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-recursive 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-recursive -1))))

(deftest test-fn-fib-recursive-cached-examples
  (is (= [0] (sut/fib-recursive-cached 0)))
  (is (= [0 1] (sut/fib-recursive-cached 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-recursive-cached 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-recursive-cached 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-recursive-cached -1))))

(deftest test-fn-fib-memoization-examples
  (is (= [0] (sut/fib-memoization 0)))
  (is (= [0 1] (sut/fib-memoization 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-memoization 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-memoization 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-memoization -1))))

(deftest test-fn-fib-binet-examples
  (is (= [0] (sut/fib-binet 0)))
  (is (= [0 1] (sut/fib-binet 1)))
  (is (= [0 1 1 2 3 5] (sut/fib-binet 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55] (sut/fib-binet 10)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-binet -1)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/fib-binet 1475))))

(deftest test-fib-values
  (testing "negative input raises"
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-iterative -1)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-memoization -1)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-binet -1))))
  (testing "matrix exponentiation (upstream doctest values)"
    (let [m [[1 1] [1 0]]]
      (is (= [[1 0] [0 1]] (sut/matrix-pow-np m 0)))
      (is (= [[1 1] [1 0]] (sut/matrix-pow-np m 1)))
      (is (= [[8 5] [5 3]] (sut/matrix-pow-np m 5)))
      (is (thrown? clojure.lang.ExceptionInfo (sut/matrix-pow-np m -1)))))
  (testing "fibonacci via matrix exponentiation"
    (is (= 0 (sut/fib-matrix-np 0)))
    (is (= 1 (sut/fib-matrix-np 1)))
    (is (= 5 (sut/fib-matrix-np 5)))
    (is (= 55 (sut/fib-matrix-np 10)))
    (is (= 832040 (sut/fib-matrix-np 30)))
    (is (thrown? clojure.lang.ExceptionInfo (sut/fib-matrix-np -1))))
  (testing "time-func returns the result and prints the timing"
    (is (= 5 (sut/time-func (fn [x] x) 5)))
    (let [out (with-out-str (sut/time-func + 1 2 3))]
      (is (= 6 (sut/time-func + 1 2 3)))
      (is (re-find #"runtime: \d+\.\d{4} (s|ms|µs)" out)))))
