(ns clojure-algorithms.project-euler.problem-131.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-131.sol1 :refer [is-prime
                                                                       solution]]))

(deftest is-prime-test
  (is (true? (is-prime 3)))
  (is (false? (is-prime 4))))

(deftest solution-test
  (is (= 4 (solution 100))))
