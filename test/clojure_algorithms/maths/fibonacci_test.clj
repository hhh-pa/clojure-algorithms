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
