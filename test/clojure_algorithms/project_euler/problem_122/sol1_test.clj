(ns clojure-algorithms.project-euler.problem-122.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-122.sol1 :refer [solve
                                                                       solution]]))

(deftest solve-test
  (is (true? (solve [1] 2 2)))
  (is (false? (solve [1] 2 0))))

(deftest solution-test
  (is (= 0 (solution 1)))
  (is (= 1 (solution 2)))
  (is (= 45 (solution 14)))
  (is (= 50 (solution 15))))
