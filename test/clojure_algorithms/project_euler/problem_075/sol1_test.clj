(ns clojure-algorithms.project-euler.problem-075.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-075.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 6 (solution 50)))
  (is (= 112 (solution 1000)))
  (is (= 5502 (solution 50000))))
