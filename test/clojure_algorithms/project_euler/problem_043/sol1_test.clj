(ns clojure-algorithms.project-euler.problem-043.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-043.sol1 :refer [substring-divisible?]]))

(deftest substring-divisible?-test
  (is (false? (substring-divisible? [0 1 2 4 6 5 7 3 8 9])))
  (is (false? (substring-divisible? [5 1 2 4 6 0 7 8 3 9])))
  (is (true? (substring-divisible? [1 4 0 6 3 5 7 2 8 9]))))
