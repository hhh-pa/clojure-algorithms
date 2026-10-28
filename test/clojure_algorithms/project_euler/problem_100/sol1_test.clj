(ns clojure-algorithms.project-euler.problem-100.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-100.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 3 (solution 2)))
  (is (= 15 (solution 4)))
  (is (= 85 (solution 21))))
