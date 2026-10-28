(ns clojure-algorithms.project-euler.problem-012.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-012.sol2
             :refer [count-divisors solution triangle-number-generator]]))

(deftest triangle-number-generator-test
  (is (= [1 3 6 10 15] (take 5 (triangle-number-generator)))))

(deftest count-divisors-test
  (is (= 1 (count-divisors 1)))
  (is (= 6 (count-divisors 28))))

(deftest solution-test
  ;; The full-scale default run is slow, so only a tiny target is tested.
  (is (= 28 (solution 5)))
  (is (= 6 (solution 3))))
