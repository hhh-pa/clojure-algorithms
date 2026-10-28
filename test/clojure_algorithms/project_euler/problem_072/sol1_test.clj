(ns clojure-algorithms.project-euler.problem-072.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-072.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 31 (solution 10)))
  (is (= 3043 (solution 100)))
  (is (= 304191 (solution 1000))))
