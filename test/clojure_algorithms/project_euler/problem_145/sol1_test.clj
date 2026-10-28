(ns clojure-algorithms.project-euler.problem-145.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-145.sol1 :refer [reversible-numbers
                                                                       slow-reversible-numbers
                                                                       slow-solution
                                                                       solution]]))

(deftest slow-reversible-numbers-test
  (is (= 0 (slow-reversible-numbers 1 0 [0] 1)))
  (is (= 20 (slow-reversible-numbers 2 0 [0 0] 2)))
  (is (= 100 (slow-reversible-numbers 3 0 [0 0 0] 3))))

(deftest slow-solution-test
  (is (= 120 (slow-solution 3)))
  (is (= 18720 (slow-solution 6))))

(deftest reversible-numbers-test
  (is (= 0 (reversible-numbers 1 0 [0] 1)))
  (is (= 20 (reversible-numbers 2 0 [0 0] 2)))
  (is (= 100 (reversible-numbers 3 0 [0 0 0] 3))))

(deftest solution-test
  (is (= 120 (solution 3)))
  (is (= 18720 (solution 6)))
  (is (= 68720 (solution 7))))
