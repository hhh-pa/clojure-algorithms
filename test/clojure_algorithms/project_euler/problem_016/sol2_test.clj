(ns clojure-algorithms.project-euler.problem-016.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-016.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 1366 (solution 1000)))
  (is (= 76 (solution 50)))
  (is (= 31 (solution 20)))
  (is (= 26 (solution 15))))
