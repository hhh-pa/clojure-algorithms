(ns clojure-algorithms.other.maximum-subsequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.maximum-subsequence :as m]))

(deftest max-subsequence-sum-test
  (testing "doctest examples"
    (is (= 10 (m/max-subsequence-sum [1 2 3 4 -2])))
    (is (= -1 (m/max-subsequence-sum [-2 -3 -1 -4 -6]))))
  (testing "empty input throws"
    (is (thrown? IllegalArgumentException (m/max-subsequence-sum [])))
    (is (thrown? IllegalArgumentException (m/max-subsequence-sum nil))))
  (testing "single element"
    (is (= 7 (m/max-subsequence-sum [7])))))
