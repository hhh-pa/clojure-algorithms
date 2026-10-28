(ns clojure-algorithms.project-euler.problem-072.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-072.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 21 (solution 8)))
  (is (= 304191 (solution 1000))))
