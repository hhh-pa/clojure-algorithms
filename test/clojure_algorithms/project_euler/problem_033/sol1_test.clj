(ns clojure-algorithms.project-euler.problem-033.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-033.sol1 :refer [digit-cancelling? fraction-list solution]]))

(deftest digit-cancelling?-test
  (is (true? (digit-cancelling? 49 98)))
  (is (false? (digit-cancelling? 30 50))))

(deftest fraction-list-test
  (is (= ["16/64" "19/95" "26/65" "49/98"] (fraction-list 2)))
  (is (= [] (fraction-list 0))))

(deftest solution-test
  (is (= 100 (solution 2))))
