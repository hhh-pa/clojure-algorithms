(ns clojure-algorithms.project-euler.problem-025.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-025.sol1
             :refer [fibonacci fibonacci-digits-index solution]]))

(deftest fibonacci-test
  (is (= 1 (fibonacci 2)))
  (is (= 2 (fibonacci 3)))
  (is (= 5 (fibonacci 5)))
  (is (= 55 (fibonacci 10)))
  (is (= 144 (fibonacci 12))))

(deftest fibonacci-digits-index-test
  (is (= 237 (fibonacci-digits-index 50)))
  (is (= 12 (fibonacci-digits-index 3))))

(deftest solution-test
  (is (= 237 (solution 50)))
  (is (= 12 (solution 3))))
