(ns clojure-algorithms.project-euler.problem-086.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-086.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 24 (solution 100)))
  (is (= 72 (solution 1000)))
  (is (= 100 (solution 2000)))
  (is (= 288 (solution 20000))))
