(ns clojure-algorithms.project-euler.problem-005.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-005.sol2 :refer [lcm solution]]))

(deftest lcm-test
  (is (= 15 (lcm 3 15)))
  (is (= 27 (lcm 1 27)))
  (is (= 351 (lcm 13 27)))
  (is (= 192 (lcm 64 48))))

(deftest solution-test
  (is (= 2520 (solution 10)))
  (is (= 360360 (solution 15)))
  (is (= 232792560 (solution 22))))
