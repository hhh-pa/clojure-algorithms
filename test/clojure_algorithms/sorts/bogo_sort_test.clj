(ns clojure-algorithms.sorts.bogo-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bogo-sort :refer [bogo-sort]]))

(deftest test-bogo-sort
  (testing "tiny inputs"
    (is (= [] (bogo-sort [])))
    (is (= [1] (bogo-sort [1])))
    (is (= [0 2 2 3 5] (bogo-sort [0 5 3 2 2])))
    (is (= [-45 -5 -2] (bogo-sort [-2 -5 -45])))
    (is (= ["a" "b" "c"] (bogo-sort ["c" "a" "b"])))
    (is (= [-1.0 0.0 2.5] (bogo-sort [2.5 -1.0 0.0])))))
