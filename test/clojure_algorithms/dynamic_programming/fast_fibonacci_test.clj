(ns clojure-algorithms.dynamic-programming.fast-fibonacci-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.fast-fibonacci :as ff]))

(deftest fast-fibonacci-test
  (testing "fast doubling Fibonacci"
    (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144]
           (mapv ff/fibonacci (range 13))))
    (is (= 0 (ff/fibonacci 0)))
    (is (= 12586269025 (ff/fibonacci 50)))
    (is (thrown? IllegalArgumentException (ff/fibonacci -1)))))
