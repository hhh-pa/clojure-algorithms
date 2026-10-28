(ns clojure-algorithms.sorts.slowsort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.slowsort :refer [slowsort]]))

(deftest test-slowsort
  (testing "doctest examples"
    (is (= [1 2 3 4 4 5 5 6]
           (slowsort [1 6 2 5 3 4 4 5])))
    (is (= ["a" "b" "c"] (slowsort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (slowsort [2.5 -1 0.0])))
    (is (= [] (slowsort [])))
    (is (= [2] (slowsort [2])))
    (is (= [1 2 3 4] (slowsort [1 2 3 4])))
    (is (= [1 2 3 4] (slowsort [4 3 2 1]))))
  (testing "range arguments"
    (is (= [9 8 2 3 4 5 6 7 1 0]
           (slowsort [9 8 7 6 5 4 3 2 1 0] 2 7)))
    (is (= [5 6 7 8 9 4 3 2 1 0]
           (slowsort [9 8 7 6 5 4 3 2 1 0] 0 4)))
    (is (= [9 8 7 6 5 0 1 2 3 4]
           (slowsort [9 8 7 6 5 4 3 2 1 0] 5 9)))))
