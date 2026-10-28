(ns clojure-algorithms.project-euler.problem-041.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-041.sol1 :refer [prime? solution]]))

(deftest prime?-test
  (is (false? (prime? 27)))
  (is (true? (prime? 563)))
  (is (false? (prime? 67483))))

(deftest solution-test
  (is (= 0 (solution 2)))
  (is (= 4231 (solution 4))))
