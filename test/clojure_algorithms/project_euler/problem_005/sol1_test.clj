(ns clojure-algorithms.project-euler.problem-005.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-005.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 2520 (solution 10)))
  (is (= 360360 (solution 15)))
  (is (= 6 (solution 3.4)))
  (is (thrown? clojure.lang.ExceptionInfo (solution 0)))
  (is (thrown? clojure.lang.ExceptionInfo (solution -17)))
  (is (thrown? clojure.lang.ExceptionInfo (solution [])))
  (is (thrown? clojure.lang.ExceptionInfo (solution "asd"))))
