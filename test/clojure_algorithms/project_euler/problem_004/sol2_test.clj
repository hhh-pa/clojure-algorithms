(ns clojure-algorithms.project-euler.problem-004.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-004.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 19591 (solution 20000)))
  (is (= 29992 (solution 30000)))
  (is (= 39893 (solution 40000))))
