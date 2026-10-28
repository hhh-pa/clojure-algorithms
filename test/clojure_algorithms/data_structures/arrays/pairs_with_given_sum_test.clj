(ns clojure-algorithms.data-structures.arrays.pairs-with-given-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.pairs-with-given-sum
             :refer [pairs-with-sum]]))

(deftest test-pairs-with-sum
  (is (= 2 (pairs-with-sum [1 5 7 1] 6)))
  (is (= 28 (pairs-with-sum [1 1 1 1 1 1 1 1] 2)))
  (is (= 4 (pairs-with-sum [1 7 6 2 5 4 3 1 9 8] 7)))
  (is (= 0 (pairs-with-sum [] 5)))
  (is (= 0 (pairs-with-sum [1] 1))))
