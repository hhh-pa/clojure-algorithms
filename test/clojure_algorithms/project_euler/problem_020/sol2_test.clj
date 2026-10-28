(ns clojure-algorithms.project-euler.problem-020.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-020.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 648 (solution 100)))
  (is (= 216 (solution 50)))
  (is (= 27 (solution 10)))
  (is (= 3 (solution 5)))
  (is (= 6 (solution 3)))
  (is (= 2 (solution 2)))
  (is (= 1 (solution 1))))
