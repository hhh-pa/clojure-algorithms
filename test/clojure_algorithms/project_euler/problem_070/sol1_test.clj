(ns clojure-algorithms.project-euler.problem-070.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-070.sol1 :refer [get-totients
                                                                       has-same-digits
                                                                       solution]]))

(deftest get-totients-test
  (is (= [0 1 1 2 2] (get-totients 5)))
  (is (= [0 1 1 2 2 4 2 6 4 6] (get-totients 10))))

(deftest has-same-digits-test
  (is (true? (has-same-digits 123456789 987654321)))
  (is (false? (has-same-digits 123 23)))
  (is (false? (has-same-digits 1234566 123456))))

(deftest solution-test
  (is (= 21 (solution 100))))
