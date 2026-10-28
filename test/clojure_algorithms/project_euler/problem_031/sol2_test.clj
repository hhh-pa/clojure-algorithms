(ns clojure-algorithms.project-euler.problem-031.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-031.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 11 (solution 10)))
  (is (= 451 (solution 50)))
  (is (= 73682 (solution 200))))
