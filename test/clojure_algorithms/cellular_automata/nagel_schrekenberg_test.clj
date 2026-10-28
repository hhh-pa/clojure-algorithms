(ns clojure-algorithms.cellular-automata.nagel-schrekenberg-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.cellular-automata.nagel-schrekenberg :as n]))
(deftest construct-test
  (is (= [[6 -1 6 -1 6 -1 6 -1 6 -1]] (n/construct-highway 10 2 6)))
  (is (= [[2 -1 -1 -1 -1 -1 -1 -1 -1 -1]] (n/construct-highway 10 10 2))))
(deftest distance-test
  (is (= 1 (n/get-distance [6 -1 6 -1 6] 2)))
  (is (= 3 (n/get-distance [2 -1 -1 -1 3 1 0 1 3 2] 0)))
  (is (= 4 (n/get-distance [-1 -1 -1 -1 2 -1 -1 -1 3] -1))))
(deftest update-test
  (is (= [-1 -1 -1 -1 -1 3 -1 -1 -1 -1 4]
         (n/update-speeds [-1 -1 -1 -1 -1 2 -1 -1 -1 -1 3] 0.0 5))))
(deftest simulate-test
  (is (= [[-1 2 -1 -1 -1 3] [-1 -1 -1 2 -1 0] [1 -1 -1 0 -1 -1]]
         (n/simulate [[-1 2 -1 -1 -1 3]] 2 0.0 3))))
