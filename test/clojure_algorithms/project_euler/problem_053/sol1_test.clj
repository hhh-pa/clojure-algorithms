(ns clojure-algorithms.project-euler.problem-053.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-053.sol1 :refer [combinations solution]]))

(deftest combinations-test
  (is (= 10 (combinations 5 3))))

(deftest solution-test
  (is (= 4075 (solution))))
