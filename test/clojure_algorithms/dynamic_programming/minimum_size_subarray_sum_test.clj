(ns clojure-algorithms.dynamic-programming.minimum-size-subarray-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.minimum-size-subarray-sum :as ms]))

(deftest minimum-subarray-sum-test
  (testing "shortest lengths"
    (is (= 2 (ms/minimum-subarray-sum 7 [2 3 1 2 4 3])))
    (is (= 4 (ms/minimum-subarray-sum 7 [2 3 -1 2 4 -3])))
    (is (= 0 (ms/minimum-subarray-sum 11 [1 1 1 1 1 1 1 1])))
    (is (= 2 (ms/minimum-subarray-sum 10 [1 2 3 4 5 6 7])))
    (is (= 1 (ms/minimum-subarray-sum 5 [1 1 1 1 1 5])))
    (is (= 0 (ms/minimum-subarray-sum 0 [])))
    (is (= 1 (ms/minimum-subarray-sum 0 [1 2 3])))
    (is (= 1 (ms/minimum-subarray-sum 10 [10 20 30])))
    (is (= 1 (ms/minimum-subarray-sum 7 [1 1 1 1 1 1 10])))
    (is (= 0 (ms/minimum-subarray-sum 6 [])))
    (is (= 1 (ms/minimum-subarray-sum 2 [1 2 3])))
    (is (= 0 (ms/minimum-subarray-sum -6 [])))
    (is (= 1 (ms/minimum-subarray-sum -6 [3 4 5])))
    (is (= 0 (ms/minimum-subarray-sum 8 nil))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ms/minimum-subarray-sum 2 "ABC")))))
