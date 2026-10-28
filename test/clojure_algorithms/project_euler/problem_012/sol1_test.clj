(ns clojure-algorithms.project-euler.problem-012.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-012.sol1
             :refer [count-divisors solution]]))

(deftest count-divisors-test
  (is (= 1 (count-divisors 1)))
  (is (= 2 (count-divisors 3)))
  (is (= 4 (count-divisors 6)))
  (is (= 6 (count-divisors 28))))

(deftest solution-test
  ;; The full-scale default run is slow, so only a tiny target is tested.
  (is (= 28 (solution 5)))
  (is (= 6 (solution 3))))
