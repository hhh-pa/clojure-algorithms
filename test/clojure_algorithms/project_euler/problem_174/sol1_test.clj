(ns clojure-algorithms.project-euler.problem-174.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-174.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 222 (solution 1000 5)))
  (is (= 249 (solution 1000 10)))
  (is (= 2383 (solution 10000 10))))
