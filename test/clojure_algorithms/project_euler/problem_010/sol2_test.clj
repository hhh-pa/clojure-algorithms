(ns clojure-algorithms.project-euler.problem-010.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-010.sol2 :refer [prime? solution]]))

(deftest prime?-test
  (is (true? (prime? 2)))
  (is (true? (prime? 3)))
  (is (false? (prime? 27)))
  (is (true? (prime? 2999)))
  (is (false? (prime? 0)))
  (is (false? (prime? 1))))

(deftest solution-test
  (is (= 76127 (solution 1000)))
  (is (= 10 (solution 7))))
