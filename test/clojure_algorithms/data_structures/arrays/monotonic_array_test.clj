(ns clojure-algorithms.data-structures.arrays.monotonic-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.monotonic-array
             :refer [is-monotonic]]))

(deftest test-is-monotonic
  (is (true? (is-monotonic [1 2 2 3])))
  (is (true? (is-monotonic [6 5 4 4])))
  (is (false? (is-monotonic [1 3 2])))
  (is (false? (is-monotonic [1 2 3 4 5 6 5])))
  (is (true? (is-monotonic [-3 -2 -1])))
  (is (true? (is-monotonic [-5 -6 -7])))
  (is (true? (is-monotonic [0 0 0])))
  (is (true? (is-monotonic [-100 0 100])))
  (is (true? (is-monotonic [7])))
  (is (true? (is-monotonic []))))
