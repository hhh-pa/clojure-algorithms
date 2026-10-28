(ns clojure-algorithms.project-euler.problem-007.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-007.sol3
             :refer [prime? prime-generator solution]]))

(deftest prime?-test
  (is (true? (prime? 2)))
  (is (true? (prime? 3)))
  (is (false? (prime? 27)))
  (is (true? (prime? 2999)))
  (is (false? (prime? 0)))
  (is (false? (prime? 1))))

(deftest prime-generator-test
  (is (= [2 3 5 7 11] (take 5 (prime-generator)))))

(deftest solution-test
  (is (= 13 (solution 6)))
  (is (= 2 (solution 1)))
  (is (= 5 (solution 3)))
  (is (= 71 (solution 20)))
  (is (= 229 (solution 50)))
  (is (= 541 (solution 100))))
