(ns clojure-algorithms.dynamic-programming.subset-generation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.subset-generation :as sg]))

(deftest subset-combinations-test
  (testing "n-element combinations"
    (is (= [[10 20] [10 30] [10 40] [20 30] [20 40] [30 40]]
           (sg/subset-combinations [10 20 30 40] 2)))
    (is (= [[1] [2] [3]] (sg/subset-combinations [1 2 3] 1)))
    (is (= [[1 2 3]] (sg/subset-combinations [1 2 3] 3)))
    (is (= [[42]] (sg/subset-combinations [42] 1)))
    (is (= [[6 7 8 9]] (sg/subset-combinations [6 7 8 9] 4)))
    (is (= [[]] (sg/subset-combinations [10 20 30 40 50] 0)))
    (is (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
           (sg/subset-combinations [1 2 3 4] 2)))
    (is (= [] (sg/subset-combinations [] 9)))
    (is (= 3 (count (sg/subset-combinations [1 "apple" 3.14] 2))))))
