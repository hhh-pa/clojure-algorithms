(ns clojure-algorithms.project-euler.problem-076.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-076.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 190569291 (solution 100)))
  (is (= 204225 (solution 50)))
  (is (= 5603 (solution 30)))
  (is (= 41 (solution 10)))
  (is (= 6 (solution 5)))
  (is (= 2 (solution 3)))
  (is (= 1 (solution 2)))
  (is (= 0 (solution 1))))
