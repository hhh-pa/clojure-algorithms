(ns clojure-algorithms.backtracking.generate-parentheses-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.generate-parentheses
             :refer [backtrack generate-parenthesis]]))

(deftest backtrack-test
  (is (= ["(())" "()()"] (backtrack "" 0 0 2))))

(deftest generate-parenthesis-test
  (testing "doctest examples"
    (is (= ["((()))" "(()())" "(())()" "()(())" "()()()"]
           (generate-parenthesis 3)))
    (is (= ["()"] (generate-parenthesis 1)))
    (is (= [""] (generate-parenthesis 0))))
  (testing "recursive and iterative versions agree as sets"
    (require 'clojure-algorithms.backtracking.generate-parentheses-iterative)
    (let [iterative @(resolve 'clojure-algorithms.backtracking.generate-parentheses-iterative/generate-parentheses-iterative)]
      (is (= (set (generate-parenthesis 4)) (set (iterative 4)))))))
