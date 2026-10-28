(ns clojure-algorithms.dynamic-programming.fibonacci-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.fibonacci :as fib]))

(deftest fibonacci-seq-test
  (testing "Fibonacci prefixes"
    (is (= [0 1 1 2 3 5 8 13 21 34] (fib/fibonacci-seq 10)))
    (is (= [0 1 1 2 3] (fib/fibonacci-seq 5)))
    (is (= [0] (fib/fibonacci-seq 1)))
    (is (= [] (fib/fibonacci-seq 0)))
    (is (= [0 1] (fib/fibonacci-seq 2)))))
