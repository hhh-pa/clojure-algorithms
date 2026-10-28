(ns clojure-algorithms.dynamic-programming.range-sum-query-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.range-sum-query :as rs]))

(deftest prefix-sum-test
  (testing "range sums"
    (is (= [81 85 63]
           (rs/prefix-sum [1 4 6 2 61 12] [[2 5] [1 5] [3 4]])))
    (is (= [9 9 7]
           (rs/prefix-sum [4 2 1 6 3] [[3 4] [1 3] [0 2]])))
    (is (= [6] (rs/prefix-sum [1 2 3] [[0 2]])))
    (is (= [] (rs/prefix-sum [1 2 3] [])))))
