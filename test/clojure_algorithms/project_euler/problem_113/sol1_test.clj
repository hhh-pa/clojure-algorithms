(ns clojure-algorithms.project-euler.problem-113.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-113.sol1 :refer [choose
                                                                       non-bouncy-exact
                                                                       non-bouncy-upto
                                                                       solution]]))

(deftest choose-test
  (is (= 6 (choose 4 2)))
  (is (= 10 (choose 5 3)))
  (is (= 38760 (choose 20 6))))

(deftest non-bouncy-exact-test
  (is (= 9 (non-bouncy-exact 1)))
  (is (= 7998 (non-bouncy-exact 6)))
  (is (= 136126 (non-bouncy-exact 10))))

(deftest non-bouncy-upto-test
  (is (= 9 (non-bouncy-upto 1)))
  (is (= 12951 (non-bouncy-upto 6)))
  (is (= 277032 (non-bouncy-upto 10))))

(deftest solution-test
  (is (= 12951 (solution 6)))
  (is (= 277032 (solution 10))))
