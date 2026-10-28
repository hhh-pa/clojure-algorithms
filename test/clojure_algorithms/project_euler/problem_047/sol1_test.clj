(ns clojure-algorithms.project-euler.problem-047.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-047.sol1 :refer [unique-prime-factors upf-len equality run solution]]))

(deftest unique-prime-factors-test
  (is (= #{2 7} (unique-prime-factors 14)))
  (is (= #{2 7 23} (unique-prime-factors 644)))
  (is (= #{2 17 19} (unique-prime-factors 646))))

(deftest upf-len-test
  (is (= 2 (upf-len 14))))

(deftest equality-test
  (is (false? (equality [1 2 3 4])))
  (is (true? (equality [2 2 2 2])))
  (is (false? (equality [1 2 3 2 1]))))

(deftest run-test
  (is (= [14 15] (run 2)))
  (is (= [644 645 646] (run 3))))

(deftest solution-test
  (is (= 14 (solution 2))))
