(ns clojure-algorithms.project-euler.problem-025.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-025.sol2
             :refer [fibonacci-generator solution]]))

(deftest fibonacci-generator-test
  (is (= [1 2 3 5 8] (take 5 (fibonacci-generator)))))

(deftest solution-test
  (is (= 476 (solution 100)))
  (is (= 237 (solution 50)))
  (is (= 12 (solution 3))))
