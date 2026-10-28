(ns clojure-algorithms.project-euler.problem-180.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-180.sol1 :refer [add-three
                                                                       is-sq
                                                                       solution]]))

(deftest is-sq-test
  (is (true? (is-sq 1)))
  (is (false? (is-sq 1000001)))
  (is (true? (is-sq 1000000))))

(deftest add-three-test
  (is (= [1 1] (add-three 1 3 1 3 1 3)))
  (is (= [262 55] (add-three 2 5 4 11 12 3))))

(deftest solution-test
  (is (= 296 (solution 5))))
