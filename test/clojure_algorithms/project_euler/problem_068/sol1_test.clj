(ns clojure-algorithms.project-euler.problem-068.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-068.sol1 :refer [generate-gon-ring
                                                                       is-magic-gon
                                                                       solution]]))

(deftest generate-gon-ring-test
  (is (= [4 2 3 5 3 1 6 1 2] (generate-gon-ring 3 [4 2 3 5 1 6])))
  (is (= [6 5 4 3 4 2 1 2 7 8 7 9 10 9 5]
         (generate-gon-ring 5 [6 5 4 3 2 1 7 8 9 10]))))

(deftest is-magic-gon-test
  (is (true? (is-magic-gon [4 2 3 5 3 1 6 1 2])))
  (is (true? (is-magic-gon [4 3 2 6 2 1 5 1 3])))
  (is (true? (is-magic-gon [2 3 5 4 5 1 6 1 3])))
  (is (false? (is-magic-gon [1 2 3 4 5 6 7 8 9])))
  (is (thrown? IllegalArgumentException (is-magic-gon [1]))))

(deftest solution-test
  (is (= 432621513 (solution 3)))
  (is (thrown? IllegalArgumentException (solution 6))))
