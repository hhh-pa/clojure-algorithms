(ns clojure-algorithms.project-euler.problem-009.sol4-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-009.sol4
             :refer [get-squares solution]]))

(deftest get-squares-test
  (is (= [] (get-squares 0)))
  (is (= [0] (get-squares 1)))
  (is (= [0 1] (get-squares 2)))
  (is (= [0 1 4] (get-squares 3)))
  (is (= [0 1 4 9] (get-squares 4))))

(deftest solution-test
  (is (= 60 (solution 12)))
  (is (= 1620 (solution 36))))
