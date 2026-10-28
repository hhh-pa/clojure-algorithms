(ns clojure-algorithms.project-euler.problem-097.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-097.sol1 :refer [solution]]))

(deftest solution-test
  (is (= "8739992577" (solution)))
  (is (= "39992577" (solution 8)))
  (is (= "7" (solution 1)))
  (is (thrown? IllegalArgumentException (solution -1)))
  (is (thrown? IllegalArgumentException (solution 8.3)))
  (is (thrown? IllegalArgumentException (solution "a"))))
