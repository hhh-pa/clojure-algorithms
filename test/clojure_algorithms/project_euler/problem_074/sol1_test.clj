(ns clojure-algorithms.project-euler.problem-074.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-074.sol1 :refer [chain-length
                                                                       solution
                                                                       sum-digit-factorials]]))

(deftest sum-digit-factorials-test
  (is (= 145 (sum-digit-factorials 145)))
  (is (= 871 (sum-digit-factorials 45361)))
  (is (= 145 (sum-digit-factorials 540))))

(deftest chain-length-test
  (is (= 11 (chain-length 10101)))
  (is (= 20 (chain-length 555)))
  (is (= 39 (chain-length 178924))))

(deftest solution-test
  (is (= 28 (solution 10 1000))))
