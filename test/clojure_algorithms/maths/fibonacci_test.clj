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
