(ns clojure-algorithms.project-euler.problem-056.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-056.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 45 (solution 10 10)))
  (is (= 972 (solution 100 100))))
