(ns clojure-algorithms.project-euler.problem-551.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-551.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 1 (solution 1)))
  (is (= 62 (solution 10)))
  (is (= 1205 (solution 100))))
