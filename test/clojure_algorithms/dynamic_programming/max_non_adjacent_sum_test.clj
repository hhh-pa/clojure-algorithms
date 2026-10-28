(ns clojure-algorithms.dynamic-programming.max-non-adjacent-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.max-non-adjacent-sum :as mn]))

(deftest max-non-adjacent-sum-test
  (testing "maximum non-adjacent sums"
    (is (= 4 (mn/maximum-non-adjacent-sum [1 2 3])))
    (is (= 18 (mn/maximum-non-adjacent-sum [1 5 3 7 2 2 6])))
    (is (= 0 (mn/maximum-non-adjacent-sum [-1 -5 -3 -7 -2 -2 -6])))
    (is (= 500 (mn/maximum-non-adjacent-sum [499 500 -3 -7 -2 -2 -6])))
    (is (= 0 (mn/maximum-non-adjacent-sum [])))
    (is (= 7 (mn/maximum-non-adjacent-sum [7])))))
