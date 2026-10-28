(ns clojure-algorithms.project-euler.problem-002.sol4-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-002.sol4 :refer [solution]]))

(deftest solution-test
  (is (= 10 (solution 10)))
  (is (= 10 (solution 15)))
  (is (= 2 (solution 2)))
  (is (= 0 (solution 1)))
  (is (= 44 (solution 34)))
  (is (= 2 (solution 3.4)))
  (is (thrown? clojure.lang.ExceptionInfo (solution 0)))
  (is (thrown? clojure.lang.ExceptionInfo (solution -17)))
  (is (thrown? clojure.lang.ExceptionInfo (solution [])))
  (is (thrown? clojure.lang.ExceptionInfo (solution "asd"))))
