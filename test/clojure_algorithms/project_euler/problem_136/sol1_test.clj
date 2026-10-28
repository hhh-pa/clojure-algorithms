(ns clojure-algorithms.project-euler.problem-136.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-136.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 0 (solution 3)))
  (is (= 3 (solution 10)))
  (is (= 25 (solution 100)))
  (is (= 27 (solution 110))))
