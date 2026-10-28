(ns clojure-algorithms.project-euler.problem-006.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-006.sol3 :refer [solution]]))

(deftest solution-test
  (is (= 2640 (solution 10)))
  (is (= 13160 (solution 15)))
  (is (= 41230 (solution 20)))
  (is (= 1582700 (solution 50))))
