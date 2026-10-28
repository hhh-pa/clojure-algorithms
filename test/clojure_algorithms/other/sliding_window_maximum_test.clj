(ns clojure-algorithms.other.sliding-window-maximum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.sliding-window-maximum :as s]))

(deftest sliding-window-maximum-test
  (testing "doctest examples"
    (is (= [3 3 5 5 6 7]
           (s/sliding-window-maximum [1 3 -1 -3 5 3 6 7] 3)))
    (is (= [11] (s/sliding-window-maximum [9 11] 2)))
    (is (= [] (s/sliding-window-maximum [] 3)))
    (is (= [4 2 12 3] (s/sliding-window-maximum [4 2 12 3] 1)))
    (is (= [1] (s/sliding-window-maximum [1] 1))))
  (testing "non-positive window throws"
    (is (thrown? IllegalArgumentException (s/sliding-window-maximum [1 2] 0)))))
