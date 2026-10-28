(ns clojure-algorithms.greedy-methods.minimum-coin-change-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.minimum-coin-change :refer [find-minimum-change]]))

(deftest test-find-minimum-change
  (is (= [2000 2000 2000 2000 2000 2000 2000 2000 2000 500 200 20 20 5]
         (find-minimum-change [1 5 10 20 50 100 200 500 1000 2000] 18745)))
  (is (= [500 100 100 100 100 50 20 10 5 2]
         (find-minimum-change [1 2 5 10 20 50 100 500 2000] 987)))
  (is (= [] (find-minimum-change [1 2 5 10 20 50 100 500 2000] 0)))
  (is (= [] (find-minimum-change [1 2 5 10 20 50 100 500 2000] -98)))
  (is (= [100 100 100 100 5 5 5 5 5 5 5 5 5 5 5 1]
         (find-minimum-change [1 5 100 500 1000] 456))))
