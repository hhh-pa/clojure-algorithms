(ns clojure-algorithms.project-euler.problem-069.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-069.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 6 (solution 10)))
  (is (= 30 (solution 100)))
  (is (= 2310 (solution 9973)))
  (is (thrown? IllegalArgumentException (solution 0))))
