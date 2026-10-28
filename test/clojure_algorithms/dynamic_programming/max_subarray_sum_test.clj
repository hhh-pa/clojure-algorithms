(ns clojure-algorithms.dynamic-programming.max-subarray-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.max-subarray-sum :as ms]))

(deftest max-subarray-sum-test
  (testing "Kadane's algorithm"
    (is (= 19 (ms/max-subarray-sum [2 8 9])))
    (is (= 0 (ms/max-subarray-sum [0 0])))
    (is (= 1.0 (ms/max-subarray-sum [-1.0 0.0 1.0])))
    (is (= 10 (ms/max-subarray-sum [1 2 3 4 -2])))
    (is (= 6 (ms/max-subarray-sum [-2 1 -3 4 -1 2 1 -5 4])))
    (is (= 8 (ms/max-subarray-sum [2 3 -9 8 -2])))
    (is (= -1 (ms/max-subarray-sum [-2 -3 -1 -4 -6])))
    (is (= 0 (ms/max-subarray-sum [-2 -3 -1 -4 -6] true)))
    (is (= 0 (ms/max-subarray-sum [])))))
