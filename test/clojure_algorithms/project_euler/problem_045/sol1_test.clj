(ns clojure-algorithms.project-euler.problem-045.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-045.sol1 :refer [hexagonal-num pentagonal? solution]]))

(deftest hexagonal-num-test
  (is (= 40755 (hexagonal-num 143)))
  (is (= 861 (hexagonal-num 21)))
  (is (= 190 (hexagonal-num 10))))

(deftest pentagonal?-test
  (is (true? (pentagonal? 330)))
  (is (false? (pentagonal? 7683)))
  (is (true? (pentagonal? 2380))))

(deftest solution-test
  (is (= 1533776805 (solution 144))))
