(ns clojure-algorithms.project-euler.problem-028.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-028.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 537 (solution 10)))
  (is (= 79697 (solution 50)))
  (is (= 651897 (solution 100))))
