(ns clojure-algorithms.greedy-methods.optimal-merge-pattern-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.optimal-merge-pattern :refer [optimal-merge-pattern]]))

(deftest test-optimal-merge-pattern
  (is (= 14 (optimal-merge-pattern [2 3 4])))
  (is (= 205 (optimal-merge-pattern [5 10 20 30 30])))
  (is (= 96 (optimal-merge-pattern [8 8 8 8 8])))
  (is (= 0 (optimal-merge-pattern [5]))))
