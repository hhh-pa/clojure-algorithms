(ns clojure-algorithms.graphs.minimum-path-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-path-sum
             :refer [min-path-sum fill-row]]))

(deftest test-minimum-path-sum
  (is (= [3 4 5] (fill-row [2 2 2] [1 2 3])))
  (is (= 7 (min-path-sum [[1 3 1] [1 5 1] [4 2 1]])))
  (is (= 20 (min-path-sum [[1 0 5 6 7] [8 9 0 4 2] [4 4 4 5 1]
                           [9 6 3 1 0] [8 4 3 2 7]])))
  (is (thrown? IllegalArgumentException (min-path-sum nil)))
  (is (thrown? IllegalArgumentException (min-path-sum [[]]))))
