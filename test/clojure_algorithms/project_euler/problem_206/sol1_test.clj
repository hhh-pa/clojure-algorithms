(ns clojure-algorithms.project-euler.problem-206.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-206.sol1 :refer [is-square-form
                                                                       solution]]))

(deftest is-square-form-test
  (is (false? (is-square-form 1)))
  (is (true? (is-square-form 112233445566778899)))
  (is (false? (is-square-form 123456789012345678))))

(deftest solution-test
  (is (= 1389019170 (solution))))
