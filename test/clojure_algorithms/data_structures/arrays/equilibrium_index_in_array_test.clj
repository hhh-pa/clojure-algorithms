(ns clojure-algorithms.data-structures.arrays.equilibrium-index-in-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.equilibrium-index-in-array
             :refer [equilibrium-index]]))

(deftest test-equilibrium-index
  (is (= -1 (equilibrium-index [])))
  (is (= 0 (equilibrium-index [5])))
  (is (= 3 (equilibrium-index [-7 1 5 2 -4 3 0])))
  (is (= -1 (equilibrium-index [2 4 6 8 10 3])))
  (is (= -1 (equilibrium-index [1 2 3 4 5])))
  (is (= 2 (equilibrium-index [1 1 1 1 1])))
  (is (= 0 (equilibrium-index [0 0 0])))
  (is (= 1 (equilibrium-index [-1 -1 -1])))
  (is (= 2 (equilibrium-index [1 -1 0]))))
