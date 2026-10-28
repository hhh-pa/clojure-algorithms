(ns clojure-algorithms.project-euler.problem-004.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-004.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 19591 (solution 20000)))
  (is (= 29992 (solution 30000)))
  (is (= 39893 (solution 40000)))
  (is (thrown? clojure.lang.ExceptionInfo (solution 10000))))
