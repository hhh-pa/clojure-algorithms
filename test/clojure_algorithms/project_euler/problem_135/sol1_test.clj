(ns clojure-algorithms.project-euler.problem-135.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-135.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 0 (solution 100)))
  (is (= 45 (solution 10000)))
  (is (= 292 (solution 50050))))
