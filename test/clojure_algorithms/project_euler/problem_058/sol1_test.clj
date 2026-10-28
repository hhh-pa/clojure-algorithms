(ns clojure-algorithms.project-euler.problem-058.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-058.sol1 :refer [prime? solution]]))

(deftest prime?-test
  (is (false? (prime? 27)))
  (is (true? (prime? 563)))
  (is (false? (prime? 67483))))

(deftest solution-test
  (is (= 11 (solution 0.5)))
  (is (= 309 (solution 0.2))))
