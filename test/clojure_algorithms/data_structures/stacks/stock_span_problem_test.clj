(ns clojure-algorithms.data-structures.stacks.stock-span-problem-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stock-span-problem
             :refer [calculate-span]]))

(deftest test-stock-span
  (is (= [1 1 1 2 1 4 6] (calculate-span [100 80 60 70 60 75 85])))
  (is (= [1 2 3 4 5] (calculate-span [10 20 30 40 50])))
  (is (= [1 1 1 1 1] (calculate-span [50 40 30 20 10])))
  (is (= [1] (calculate-span [100]))))
